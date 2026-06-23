use super::*;

#[test]
fn test_eval_binary_add() {
    let expr = binop(rumoca_core::OpBinary::Add, lit(2.0), lit(3.0));
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 5.0);
}

#[test]
fn test_eval_unary_minus() {
    let expr = rumoca_core::Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Box::new(lit(5.0)),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), -5.0);
}

#[test]
fn test_eval_unary_not() {
    let expr = rumoca_core::Expression::Unary {
        op: rumoca_core::OpUnary::Not,
        rhs: Box::new(bool_lit(true)),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 0.0);
}

#[test]
fn test_eval_builtin_sin() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sin,
        args: vec![lit(0.0)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 0.0);
}

#[test]
fn test_eval_builtin_cos() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Cos,
        args: vec![lit(0.0)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 1.0);
}

#[test]
fn test_eval_builtin_sqrt() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sqrt,
        args: vec![lit(9.0)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 3.0);
}

#[test]
fn test_eval_builtin_mod_uses_floor_semantics() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Mod,
        args: vec![lit(-7.0), lit(3.0)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 2.0);
}

#[test]
fn test_eval_builtin_rem_keeps_truncating_semantics() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Rem,
        args: vec![lit(-7.0), lit(3.0)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), -1.0);
}

#[test]
fn test_eval_builtin_exp() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Exp,
        args: vec![lit(0.0)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 1.0);
}

#[test]
fn test_eval_builtin_abs() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Abs,
        args: vec![lit(-3.0)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 3.0);
}

#[test]
fn test_eval_builtin_min_max() {
    let min_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Min,
        args: vec![lit(3.0), lit(5.0)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&min_expr, &VarEnv::new()), 3.0);

    let max_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Max,
        args: vec![lit(3.0), lit(5.0)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&max_expr, &VarEnv::new()), 5.0);
}

#[test]
fn test_eval_builtin_one_arg_array_reductions() {
    let mut env = VarEnv::<f64>::new();
    set_vector_var(&mut env, "x", &[-2.0, 3.0, 4.0]);

    let min_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Min,
        args: vec![var("x")],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&min_expr, &env), -2.0);

    let max_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Max,
        args: vec![var("x")],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&max_expr, &env), 4.0);

    let sum_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sum,
        args: vec![var("x")],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&sum_expr, &env), 5.0);

    let product_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Product,
        args: vec![var("x")],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&product_expr, &env), -24.0);
}

#[test]
fn test_eval_builtin_max_abs_single_arg_array_reduction() {
    let mut env = VarEnv::<f64>::new();
    set_vector_var(&mut env, "aux", &[-2.0, 0.25, 3.5, -1.0]);

    let max_abs_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Max,
        args: vec![rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Abs,
            args: vec![var("aux")],
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&max_abs_expr, &env), 3.5);
}

#[test]
fn test_checked_eval_builtin_max_over_array_literal() {
    let mut env = VarEnv::<f64>::new();
    env.set("a", 0.1);
    env.set("b", 10.0);

    let max_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Max,
        args: vec![arr(
            vec![
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Div,
                    lhs: Box::new(var("a")),
                    rhs: Box::new(var("b")),
                    span: rumoca_core::Span::DUMMY,
                },
                lit(1.0e-12),
            ],
            true,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&max_expr, &env).unwrap(), 0.01);
}

#[test]
fn test_eval_builtin_sum_single_arg_array_preserves_dual_ad() {
    let mut env = VarEnv::<Dual>::new();
    set_vector_var(
        &mut env,
        "x",
        &[
            Dual::new(2.0, 1.0),
            Dual::new(3.0, 0.0),
            Dual::new(-1.0, 0.5),
        ],
    );

    let sum_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sum,
        args: vec![var("x")],
        span: rumoca_core::Span::DUMMY,
    };
    let y = eval_expr_or_default::<Dual>(&sum_expr, &env);
    assert!((y.re - 4.0).abs() < 1e-12);
    assert!((y.du - 1.5).abs() < 1e-12);
}

#[test]
fn test_eval_if_true() {
    let expr = rumoca_core::Expression::If {
        branches: vec![(bool_lit(true), lit(1.0))],
        else_branch: Box::new(lit(2.0)),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 1.0);
}

#[test]
fn test_eval_if_false() {
    let expr = rumoca_core::Expression::If {
        branches: vec![(bool_lit(false), lit(1.0))],
        else_branch: Box::new(lit(2.0)),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &VarEnv::new()), 2.0);
}

#[test]
fn test_checked_eval_field_access_projects_selected_if_branch() {
    let mut record_ctor = rumoca_core::Function::new("Pkg.Record", Default::default());
    record_ctor.add_input(rumoca_core::FunctionParam::new("x", "Real"));
    let mut env = VarEnv::<f64>::new();
    env.functions = Arc::new(IndexMap::from([("Pkg.Record".to_string(), record_ctor)]));

    let then_record = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.Record"),
        args: vec![named_ctor_arg("x", lit(1.25))],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let else_record = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.Record"),
        args: vec![named_ctor_arg("x", lit(2.5))],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::If {
            branches: vec![(bool_lit(false), then_record)],
            else_branch: Box::new(else_record),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "x".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(2.5));
}

#[test]
fn test_eval_array_field_access_projects_constructor_reference_alias() {
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Medium.setState_pTX"),
            args: vec![
                named_ctor_arg("p", lit(101325.0)),
                named_ctor_arg("T", lit(293.15)),
                named_ctor_arg("X", arr(vec![lit(0.01), lit(0.99)], false)),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        }),
        field: "reference_X".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_shaped_array_values::<f64>(&expr, &VarEnv::new(), 2),
        Ok(vec![0.01, 0.99])
    );
}

#[test]
fn test_checked_eval_var_ref_singleton_range_slice_as_scalar() {
    let mut env = VarEnv::<f64>::new();
    env.vars.insert("x[1]".to_string(), 0.42);
    env.vars.insert("x[2]".to_string(), 0.99);
    env.dims = Arc::new(IndexMap::from([("x".to_string(), vec![2])]));

    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("x"),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
            rumoca_core::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(int_lit(1)),
                span: rumoca_core::Span::DUMMY,
            },
        ))],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(0.42));
}

#[test]
fn test_checked_eval_field_access_projects_nested_constructor_field() {
    let mut inner_ctor = rumoca_core::Function::new("Pkg.Inner", Default::default());
    inner_ctor.add_input(rumoca_core::FunctionParam::new("x", "Real"));
    let mut outer_ctor = rumoca_core::Function::new("Pkg.Outer", Default::default());
    outer_ctor.add_input(rumoca_core::FunctionParam::new("inner", "Pkg.Inner"));
    let mut env = VarEnv::<f64>::new();
    env.functions = Arc::new(IndexMap::from([
        ("Pkg.Inner".to_string(), inner_ctor),
        ("Pkg.Outer".to_string(), outer_ctor),
    ]));

    let inner_record = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.Inner"),
        args: vec![named_ctor_arg("x", lit(2.5))],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let outer_record = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.Outer"),
        args: vec![named_ctor_arg("inner", inner_record)],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(outer_record),
            field: "inner".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "x".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(2.5));
}

#[test]
fn test_checked_eval_field_access_resolves_indexed_component_path() {
    let mut env = VarEnv::<f64>::new();
    env.set("zone[1].building.relativeSurfaceTolerance", 1.0e-6);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(var("zone")),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "building".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "relativeSurfaceTolerance".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(1.0e-6));
}

#[test]
fn test_checked_eval_field_access_resolves_outer_component_field_alias() {
    let mut env = VarEnv::<f64>::new();
    env.set("building.relativeSurfaceTolerance", 1.0e-6);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(var("zone")),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "building".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "relativeSurfaceTolerance".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(1.0e-6));
}

#[test]
fn test_checked_eval_field_access_resolves_enclosing_scope_field_alias() {
    let mut env = VarEnv::<f64>::new();
    env.set("unit.coi.dp1_nominal", 42.0);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(var("unit")),
                field: "coi".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            field: "cooCoi".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "dp1_nominal".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(42.0));
}

#[test]
fn test_checked_eval_field_access_resolves_pressure_drop_modifier_alias() {
    let mut env = VarEnv::<f64>::new();
    env.set("unit.coi.PreDroWat", 0.0);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(var("unit")),
                field: "coi".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            field: "cooCoi".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        field: "dp1_nominal".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(0.0));
}

#[test]
fn test_checked_eval_var_ref_resolves_pressure_drop_modifier_alias() {
    let mut env = VarEnv::<f64>::new();
    env.set("unit.coi.PreDroWat", 0.0);

    assert_eq!(
        eval_expr::<f64>(&var("unit.coi.cooCoi.dp1_nominal"), &env),
        Ok(0.0)
    );
}

#[test]
fn test_checked_eval_var_ref_resolves_enclosing_scope_alias() {
    let mut env = VarEnv::<f64>::new();
    env.set("zone[1].AFlo", 12.0);

    assert_eq!(
        eval_expr::<f64>(&var("zone[1].fmuZon.AFlo"), &env),
        Ok(12.0)
    );
}

#[test]
fn test_eval_comparison() {
    let lt = binop(rumoca_core::OpBinary::Lt, lit(1.0), lit(2.0));
    assert_eq!(eval_expr_or_default::<f64>(&lt, &VarEnv::new()), 1.0);

    let gt = binop(rumoca_core::OpBinary::Gt, lit(1.0), lit(2.0));
    assert_eq!(eval_expr_or_default::<f64>(&gt, &VarEnv::new()), 0.0);
}

#[test]
fn test_eval_der_lookup() {
    let mut env = VarEnv::<f64>::new();
    env.set("der(x)", 5.0);

    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![var("x")],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 5.0);
}

#[test]
fn test_eval_complex_expression() {
    let mut env = VarEnv::<f64>::new();
    env.set("x", 3.0);
    env.set("y", std::f64::consts::FRAC_PI_2);

    let expr = binop(
        rumoca_core::OpBinary::Mul,
        binop(rumoca_core::OpBinary::Add, lit(2.0), var("x")),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sin,
            args: vec![var("y")],
            span: rumoca_core::Span::DUMMY,
        },
    );
    let result = eval_expr_or_default::<f64>(&expr, &env);
    assert!((result - 5.0).abs() < 1e-10);
}

#[test]
fn test_eval_field_access_on_var_ref_uses_flat_field_name() {
    let mut env = VarEnv::<f64>::new();
    env.set("z.re", 2.5);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(var("z")),
        field: "re".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 2.5);
}

#[test]
fn test_eval_field_access_on_constructor_complex_components() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut complex = rumoca_core::Function::new("Complex", rumoca_core::Span::DUMMY);
    complex.is_constructor = true;
    complex.add_input(rumoca_core::FunctionParam::new("re", "Real"));
    complex.add_input(rumoca_core::FunctionParam::new("im", "Real"));
    funcs.insert("Complex".to_string(), complex);
    env.functions = std::sync::Arc::new(funcs);
    let ctor = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Complex"),
        args: vec![lit(3.0), lit(4.0)],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let re_expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(ctor.clone()),
        field: "re".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let im_expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(ctor),
        field: "im".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&re_expr, &env), 3.0);
    assert_eq!(eval_expr_or_default::<f64>(&im_expr, &env), 4.0);
}

#[test]
fn test_eval_field_access_after_array_literal_index() {
    let mut env = VarEnv::<f64>::new();
    env.set("i", 2.0);
    env.set("left.im", 4.0);
    env.set("right.im", 9.0);
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::Array {
                elements: vec![var("left"), var("right")],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(var("i")))],
            span: rumoca_core::Span::DUMMY,
        }),
        field: "im".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 9.0);
}

#[test]
fn test_eval_constructor_call_scalar_fallback_uses_first_argument() {
    let env = VarEnv::<f64>::new();
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Complex"),
        args: vec![lit(9.0), lit(1.0)],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 9.0);
}

#[test]
fn test_eval_field_access_constructor_named_args_bind_by_name() {
    let mut ctor = rumoca_core::Function::new(
        "Modelica.Electrical.Machines.Losses.CoreParameters",
        Default::default(),
    );
    ctor.add_input(rumoca_core::FunctionParam::new("m", "Integer"));
    ctor.add_input(rumoca_core::FunctionParam::new("PRef", "Real").with_default(lit(0.0)));
    ctor.add_input(rumoca_core::FunctionParam::new("VRef", "Real"));
    ctor.add_input(rumoca_core::FunctionParam::new("wRef", "Real"));
    ctor.add_input(
        rumoca_core::FunctionParam::new("GcRef", "Real").with_default(
            rumoca_core::Expression::If {
                branches: vec![(
                    rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Le,
                        lhs: Box::new(var("PRef")),
                        rhs: Box::new(lit(0.0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    lit(0.0),
                )],
                else_branch: Box::new(rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Div,
                    lhs: Box::new(rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Div,
                        lhs: Box::new(var("PRef")),
                        rhs: Box::new(rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Mul,
                            lhs: Box::new(var("VRef")),
                            rhs: Box::new(var("VRef")),
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rhs: Box::new(var("m")),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
        ),
    );

    let mut env = VarEnv::<f64>::new();
    env.functions = Arc::new(IndexMap::from([(
        "Modelica.Electrical.Machines.Losses.CoreParameters".to_string(),
        ctor,
    )]));
    env.set("m", 3.0);
    env.set("wRef", 314.0);

    let ctor_call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Modelica.Electrical.Machines.Losses.CoreParameters"),
        args: vec![
            named_ctor_arg("PRef", lit(410.0)),
            named_ctor_arg("VRef", lit(387.9)),
        ],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };

    let p_ref = rumoca_core::Expression::FieldAccess {
        base: Box::new(ctor_call.clone()),
        field: "PRef".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let v_ref = rumoca_core::Expression::FieldAccess {
        base: Box::new(ctor_call.clone()),
        field: "VRef".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let gc_ref = rumoca_core::Expression::FieldAccess {
        base: Box::new(ctor_call),
        field: "GcRef".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    let p_ref_value = eval_expr_or_default::<f64>(&p_ref, &env);
    let v_ref_value = eval_expr_or_default::<f64>(&v_ref, &env);
    let gc_ref_value = eval_expr_or_default::<f64>(&gc_ref, &env);

    assert!((p_ref_value - 410.0).abs() < 1e-12);
    assert!((v_ref_value - 387.9).abs() < 1e-12);
    assert!(gc_ref_value.is_finite() && gc_ref_value > 0.0);
}

#[test]
fn test_eval_dual_x_squared() {
    // f(x) = x*x at x=3 → f=9, f'=6
    let mut env = VarEnv::<Dual>::new();
    env.set("x", Dual::new(3.0, 1.0));

    let expr = binop(rumoca_core::OpBinary::Mul, var("x"), var("x"));
    let result = eval_expr_or_default::<Dual>(&expr, &env);
    assert!((result.re - 9.0).abs() < 1e-12);
    assert!((result.du - 6.0).abs() < 1e-12);
}

#[test]
fn test_lift_env() {
    let mut env = VarEnv::<f64>::new();
    env.set("x", 3.0);
    env.set("y", 5.0);

    let dual_env: VarEnv<Dual> = lift_env(&env);
    let x = dual_env.get("x");
    assert_eq!(x.re, 3.0);
    assert_eq!(x.du, 0.0);
}

#[test]
fn test_modelica_constants_in_build_env() {
    let dae = Dae::new();
    let env = build_env(&dae, &[], &[], 0.0);

    // Full qualified names (MLS §3.7.3 — short aliases resolved at flatten time via imports)
    assert!((env.get("Modelica.Constants.pi") - std::f64::consts::PI).abs() < 1e-15);
    assert!((env.get("Modelica.Constants.e") - std::f64::consts::E).abs() < 1e-15);
    assert!((env.get("Modelica.Constants.g_n") - 9.80665).abs() < 1e-10);
    assert!(env.get("Modelica.Constants.inf").is_infinite());
    assert!((env.get("Modelica.ComplexMath.j.re") - 0.0).abs() < 1e-15);
    assert!((env.get("Modelica.ComplexMath.j.im") - 1.0).abs() < 1e-15);
    assert!((env.get("j.re") - 0.0).abs() < 1e-15);
    assert!((env.get("j.im") - 1.0).abs() < 1e-15);
}

#[test]
fn test_modelica_constants_do_not_override_dae_values() {
    let mut dae = Dae::new();

    // Provide a custom "Modelica.Constants.pi" in the DAE constants
    let mut var = dae::Variable::new(rumoca_core::VarName::new("Modelica.Constants.pi"));
    var.start = Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(3.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables
        .constants
        .insert(rumoca_core::VarName::new("Modelica.Constants.pi"), var);

    let env = build_env(&dae, &[], &[], 0.0);
    // DAE-provided value should win over the fallback
    assert!((env.get("Modelica.Constants.pi") - 3.0).abs() < 1e-15);
}

#[test]
fn test_normalize_subscript_arithmetic() {
    let env = VarEnv::<f64>::new();
    assert_eq!(
        normalize_var_name::<f64>("x[(2 - 1)]", &env),
        Some("x[1]".to_string())
    );
    assert_eq!(
        normalize_var_name::<f64>("x[(3 + 1)]", &env),
        Some("x[4]".to_string())
    );
    assert_eq!(
        normalize_var_name::<f64>("x[(2 * 3)]", &env),
        Some("x[6]".to_string())
    );
    // Plain integer subscripts are already normalized
    assert_eq!(normalize_var_name::<f64>("x[1]", &env), None);
}

#[test]
fn test_normalize_subscript_variable() {
    let mut env = VarEnv::<f64>::new();
    env.set("n", 2.0);
    assert_eq!(
        normalize_var_name::<f64>("x[n]", &env),
        Some("x[2]".to_string())
    );
}

#[test]
fn test_eval_var_ref_subscript_expression() {
    let mut env = VarEnv::<f64>::new();
    env.set("x[1]", 42.0);
    env.set("x[2]", 99.0);
    env.set("n", 2.0);
    // x[(2 - 1)] should resolve to x[1] = 42.0
    assert_eq!(eval_var_ref_no_subscripts::<f64>("x[(2 - 1)]", &env), 42.0);
    // x[n] should resolve to x[2] = 99.0
    assert_eq!(eval_var_ref_no_subscripts::<f64>("x[n]", &env), 99.0);
}

#[test]
fn test_eval_var_ref_unity_subscript_falls_back_to_base_name() {
    let mut env = VarEnv::<f64>::new();
    env.set("arr", 7.5);
    assert_eq!(eval_var_ref_no_subscripts::<f64>("arr[1]", &env), 7.5);
}

#[test]
fn test_eval_var_ref_non_unity_subscript_does_not_fall_back_to_base_name() {
    let mut env = VarEnv::<f64>::new();
    env.set("arr", 7.5);
    assert_eq!(eval_var_ref_no_subscripts::<f64>("arr[2]", &env), 0.0);
}

#[test]
fn test_eval_function_call_requires_exact_key_for_qualified_call() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    funcs.insert(
        "localFn".to_string(),
        user_function_with_default_output("localFn", 7.0),
    );
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.localFn"),
        args: vec![],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert!(eval_expr_or_default::<f64>(&expr, &env).is_nan());
}

#[test]
fn test_eval_function_call_requires_exact_key_for_short_call() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    funcs.insert(
        "Pkg.localFn".to_string(),
        user_function_with_default_output("Pkg.localFn", 11.0),
    );
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("localFn"),
        args: vec![],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert!(eval_expr_or_default::<f64>(&expr, &env).is_nan());
}

#[test]
fn test_eval_function_call_matches_exact_user_key() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    funcs.insert(
        "Pkg.localFn".to_string(),
        user_function_with_default_output("Pkg.localFn", 11.0),
    );
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.localFn"),
        args: vec![],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 11.0);
}

#[test]
fn test_eval_function_call_named_args_bind_by_name() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = rumoca_core::Function::new("Pkg.affine", Default::default());
    f.add_input(rumoca_core::FunctionParam::new("u", "Real"));
    f.add_input(rumoca_core::FunctionParam::new("A", "Real"));
    f.add_input(rumoca_core::FunctionParam::new("w", "Real"));
    f.add_output(
        rumoca_core::FunctionParam::new("y", "Real").with_default(binop(
            rumoca_core::OpBinary::Add,
            binop(rumoca_core::OpBinary::Add, var("u"), var("A")),
            var("w"),
        )),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.affine".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call(
        "Pkg.affine",
        vec![
            named_ctor_arg("w", lit(3.0)),
            named_ctor_arg("u", lit(2.0)),
            named_ctor_arg("A", lit(5.0)),
        ],
    );
    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 10.0);
}

#[test]
fn test_eval_function_call_defaults_can_reference_prior_inputs() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = rumoca_core::Function::new("Pkg.defaults", Default::default());
    f.add_input(rumoca_core::FunctionParam::new("a", "Real"));
    f.add_input(
        rumoca_core::FunctionParam::new("b", "Real").with_default(binop(
            rumoca_core::OpBinary::Add,
            var("a"),
            lit(1.0),
        )),
    );
    f.add_input(
        rumoca_core::FunctionParam::new("c", "Real").with_default(binop(
            rumoca_core::OpBinary::Add,
            var("b"),
            lit(1.0),
        )),
    );
    f.add_output(rumoca_core::FunctionParam::new("y", "Real").with_default(var("c")));
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.defaults".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call("Pkg.defaults", vec![named_ctor_arg("a", lit(2.0))]);
    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 4.0);
}

#[test]
fn test_eval_function_call_selected_outputs_and_indexed_assignments() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut random_like = rumoca_core::Function::new("Pkg.randomLike", Default::default());
    random_like.add_input(rumoca_core::FunctionParam::new("seedIn", "Integer").with_dims(vec![3]));
    random_like.add_output(rumoca_core::FunctionParam::new("x", "Real"));
    random_like
        .add_output(rumoca_core::FunctionParam::new("seedOut", "Integer").with_dims(vec![3]));
    random_like.body = vec![
        rumoca_core::Statement::Assignment {
            comp: comp_ref("x"),
            value: lit(0.25),

            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::Assignment {
            comp: comp_ref_index("seedOut", 1),
            value: int_lit(11),

            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::Assignment {
            comp: comp_ref_index("seedOut", 2),
            value: int_lit(22),

            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::Assignment {
            comp: comp_ref_index("seedOut", 3),
            value: int_lit(33),

            span: rumoca_core::Span::DUMMY,
        },
    ];
    funcs.insert("Pkg.randomLike".to_string(), random_like);
    env.functions = std::sync::Arc::new(funcs);

    let args = vec![arr(vec![int_lit(1), int_lit(2), int_lit(3)], false)];
    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.randomLike", args.clone()), &env),
        0.25
    );
    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.randomLike.seedOut[2]", args), &env),
        22.0
    );
}

#[test]
fn test_eval_array_values_collects_function_array_output() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut vector = rumoca_core::Function::new("Pkg.vector", Default::default());
    vector.add_output(rumoca_core::FunctionParam::new("y", "Real").with_dims(vec![3]));
    vector.body = vec![
        rumoca_core::Statement::Assignment {
            comp: comp_ref_index("y", 1),
            value: lit(2.0),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::Assignment {
            comp: comp_ref_index("y", 2),
            value: lit(3.0),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Statement::Assignment {
            comp: comp_ref_index("y", 3),
            value: lit(5.0),
            span: rumoca_core::Span::DUMMY,
        },
    ];
    funcs.insert("Pkg.vector".to_string(), vector);
    env.functions = std::sync::Arc::new(funcs);

    assert_eq!(
        eval_array_values::<f64>(&fn_call("Pkg.vector", vec![]), &env),
        vec![2.0, 3.0, 5.0]
    );
}

#[test]
fn test_statement_constructor_assignment_materializes_record_fields() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut orientation = rumoca_core::Function::new("Pkg.Orientation", Default::default());
    orientation.add_input(rumoca_core::FunctionParam::new("T", "Real").with_dims(vec![3]));
    orientation.add_input(rumoca_core::FunctionParam::new("w", "Real").with_dims(vec![2]));
    funcs.insert("Pkg.Orientation".to_string(), orientation);
    env.functions = std::sync::Arc::new(funcs);

    crate::statement::eval_statements(
        &[rumoca_core::Statement::Assignment {
            comp: comp_ref("R"),
            value: rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.Orientation"),
                args: vec![
                    named_ctor_arg("T", arr(vec![lit(1.0), lit(2.0), lit(3.0)], false)),
                    named_ctor_arg("w", arr(vec![lit(4.0), lit(5.0)], false)),
                ],
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            },
            span: rumoca_core::Span::DUMMY,
        }],
        &mut env,
    )
    .expect("constructor assignment should evaluate");

    assert_eq!(
        eval_array_values::<f64>(&var("R.T"), &env),
        vec![1.0, 2.0, 3.0]
    );
    assert_eq!(eval_array_values::<f64>(&var("R.w"), &env), vec![4.0, 5.0]);
}

#[test]
fn test_eval_field_access_array_values_selects_record_function_output() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut orientation = rumoca_core::Function::new("Pkg.Orientation", Default::default());
    orientation.add_input(rumoca_core::FunctionParam::new("T", "Real").with_dims(vec![3]));
    orientation.add_input(rumoca_core::FunctionParam::new("w", "Real").with_dims(vec![2]));
    funcs.insert("Pkg.Orientation".to_string(), orientation);

    let mut axes = rumoca_core::Function::new("Pkg.axes", Default::default());
    axes.add_output(rumoca_core::FunctionParam::new("R", "Orientation"));
    axes.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("R"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.Orientation"),
            args: vec![
                named_ctor_arg("T", arr(vec![lit(1.0), lit(0.0), lit(0.0)], false)),
                named_ctor_arg("w", arr(vec![lit(4.0), lit(5.0)], false)),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.axes".to_string(), axes);
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(fn_call("Pkg.axes", vec![])),
        field: "T".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_array_values::<f64>(&expr, &env), vec![1.0, 0.0, 0.0]);
}

#[test]
fn test_eval_field_access_scalar_selects_record_function_output() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut params = rumoca_core::Function::new("Pkg.Params", Default::default());
    params.add_input(rumoca_core::FunctionParam::new("gain", "Real"));
    params.add_input(rumoca_core::FunctionParam::new("offset", "Real"));
    funcs.insert("Pkg.Params".to_string(), params);

    let mut make_params = rumoca_core::Function::new("Pkg.makeParams", Default::default());
    make_params.add_output(
        rumoca_core::FunctionParam::new("p", "Params")
            .with_type_class(rumoca_core::ClassType::Record),
    );
    make_params.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("p"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.Params"),
            args: vec![
                named_ctor_arg("gain", lit(42.0)),
                named_ctor_arg("offset", lit(3.0)),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.makeParams".to_string(), make_params);
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(fn_call("Pkg.makeParams", vec![])),
        field: "gain".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(42.0));
}

#[test]
fn test_eval_field_access_scalar_selects_record_function_output_field_assignment() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut make_params = rumoca_core::Function::new("Pkg.makeParams", Default::default());
    make_params.add_output(
        rumoca_core::FunctionParam::new("p", "Params")
            .with_type_class(rumoca_core::ClassType::Record),
    );
    make_params.body = vec![rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "p".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: vec![],
                },
                rumoca_core::ComponentRefPart {
                    ident: "gain".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: vec![],
                },
            ],
            def_id: None,
        },
        value: lit(42.0),
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.makeParams".to_string(), make_params);
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(fn_call("Pkg.makeParams", vec![])),
        field: "gain".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(42.0));
}

#[test]
fn test_record_function_output_preserves_matrix_product_field_shape() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut orientation = Function::new("Pkg.Orientation", Default::default());
    orientation.add_input(FunctionParam::new("T", "Real").with_dims(vec![3, 3]));
    orientation.add_input(FunctionParam::new("w", "Real").with_dims(vec![3]));
    funcs.insert("Pkg.Orientation".to_string(), orientation);

    let mut axis = Function::new("Pkg.axisRotation", Default::default());
    axis.add_output(FunctionParam::new("T", "Real").with_dims(vec![3, 3]));
    axis.body = vec![Statement::Assignment {
        comp: comp_ref("T"),
        value: arr(
            vec![
                arr(vec![lit(1.0), lit(0.0), lit(0.0)], true),
                arr(vec![lit(0.0), lit(1.0), lit(0.0)], true),
                arr(vec![lit(0.0), lit(0.0), lit(1.0)], true),
            ],
            true,
        ),
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.axisRotation".to_string(), axis);

    let matrix_product = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(fn_call("Pkg.axisRotation", vec![])),
        rhs: Box::new(fn_call("Pkg.axisRotation", vec![])),
        span: rumoca_core::Span::DUMMY,
    };
    let mut axes = Function::new("Pkg.axes", Default::default());
    axes.add_output(FunctionParam::new("R", "Orientation"));
    axes.body = vec![Statement::Assignment {
        comp: comp_ref("R"),
        value: Expression::FunctionCall {
            name: Reference::new("Pkg.Orientation"),
            args: vec![
                named_ctor_arg("T", matrix_product),
                named_ctor_arg(
                    "w",
                    builtin(rumoca_core::BuiltinFunction::Zeros, vec![int_lit(3)]),
                ),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.axes".to_string(), axes);
    env.functions = Arc::new(funcs);

    let expr = Expression::FieldAccess {
        base: Box::new(fn_call("Pkg.axes", vec![])),
        field: "T".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_array_values::<f64>(&expr, &env),
        vec![1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0]
    );
}

#[test]
fn test_shaped_matrix_product_rejects_unshaped_function_output() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut axis = Function::new("Pkg.axisRotation", Default::default());
    axis.add_output(FunctionParam::new("T", "Real"));
    axis.body = vec![Statement::Assignment {
        comp: comp_ref("T"),
        value: arr(
            vec![
                arr(vec![lit(1.0), lit(0.0), lit(0.0)], true),
                arr(vec![lit(0.0), lit(1.0), lit(0.0)], true),
                arr(vec![lit(0.0), lit(0.0), lit(1.0)], true),
            ],
            true,
        ),
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.axisRotation".to_string(), axis);
    env.functions = Arc::new(funcs);

    let expr = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(fn_call("Pkg.axisRotation", vec![])),
        rhs: Box::new(fn_call("Pkg.axisRotation", vec![])),
        span: rumoca_core::Span::DUMMY,
    };

    assert!(matches!(
        eval_shaped_array_values::<f64>(&expr, &env, 9),
        Err(EvalError::ShapeMismatch {
            context: "shaped array value",
            expected: 9,
            actual: 1,
        })
    ));
}

#[test]
fn test_function_array_input_binds_builtin_zeros_shape() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut vector = Function::new("Pkg.vector", Default::default());
    vector.add_input(FunctionParam::new("u", "Real").with_dims(vec![3]));
    vector.add_output(FunctionParam::new("y", "Real").with_dims(vec![3]));
    vector.body = vec![Statement::Assignment {
        comp: comp_ref("y"),
        value: var("u"),
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.vector".to_string(), vector);
    env.functions = Arc::new(funcs);

    assert_eq!(
        eval_array_values::<f64>(
            &fn_call(
                "Pkg.vector",
                vec![builtin(BuiltinFunction::Zeros, vec![int_lit(3)])]
            ),
            &env
        ),
        vec![0.0, 0.0, 0.0]
    );
}

#[test]
fn test_function_array_input_rejects_wrong_shape() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut vector = Function::new("Pkg.vector", Default::default());
    vector.add_input(FunctionParam::new("u", "Real").with_dims(vec![3]));
    vector.add_output(FunctionParam::new("y", "Real").with_dims(vec![3]));
    vector.body = vec![Statement::Assignment {
        comp: comp_ref("y"),
        value: var("u"),
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.vector".to_string(), vector);
    env.functions = Arc::new(funcs);

    let expr = fn_call("Pkg.vector", vec![arr(vec![lit(1.0), lit(2.0)], false)]);

    let shaped = eval_shaped_array_values::<f64>(&expr, &env, 3);
    assert_eq!(
        shaped,
        Err(EvalError::ShapeMismatch {
            context: "function array input",
            expected: 3,
            actual: 2,
        })
    );
    assert_eq!(
        eval_expr::<f64>(&expr, &env),
        Err(EvalError::ShapeMismatch {
            context: "shaped array value",
            expected: 3,
            actual: 2,
        })
    );
}

#[test]
fn test_eval_function_call_selection_suffix_rejects_malformed_indices() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut random_like = rumoca_core::Function::new("Pkg.randomLikeMalformed", Default::default());
    random_like.add_output(rumoca_core::FunctionParam::new("x", "Real"));
    random_like
        .add_output(rumoca_core::FunctionParam::new("seedOut", "Integer").with_dims(vec![3]));
    random_like.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref_index("seedOut", 1),
        value: int_lit(11),

        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.randomLikeMalformed".to_string(), random_like);
    env.functions = std::sync::Arc::new(funcs);

    assert!(
        eval_expr_or_default::<f64>(
            &fn_call("Pkg.randomLikeMalformed.seedOut[idx.re]", vec![]),
            &env
        )
        .is_nan(),
        "malformed selection suffix must not resolve through dotted bracket text"
    );
}

#[test]
fn test_eval_function_call_selected_complex_output_components() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = rumoca_core::Function::new("Pkg.powerOfJ", Default::default());
    f.add_output(rumoca_core::FunctionParam::new(
        "x",
        "Modelica.ComplexMath.Complex",
    ));
    f.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("x"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Complex"),
            args: vec![lit(2.0), lit(-3.0)],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },

        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.powerOfJ".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.powerOfJ.x.re", vec![]), &env),
        2.0
    );
    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.powerOfJ.x.im", vec![]), &env),
        -3.0
    );
}

#[test]
fn test_eval_function_call_selected_complex_output_from_single_arg_constructor() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = rumoca_core::Function::new("Pkg.singleArgComplex", Default::default());
    f.add_output(rumoca_core::FunctionParam::new(
        "x",
        "Modelica.ComplexMath.Complex",
    ));
    f.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("x"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Complex"),
            args: vec![lit(1.0)],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },

        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.singleArgComplex".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.singleArgComplex.x.re", vec![]), &env),
        1.0
    );
    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.singleArgComplex.x.im", vec![]), &env),
        0.0
    );
}

#[test]
fn test_eval_function_call_selected_complex_output_from_plain_complex_call() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    funcs.insert(
        "Complex".to_string(),
        rumoca_core::Function::new("Complex", Default::default()),
    );

    let mut f = rumoca_core::Function::new("Pkg.plainComplex", Default::default());
    f.add_output(rumoca_core::FunctionParam::new(
        "x",
        "Modelica.ComplexMath.Complex",
    ));
    f.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("x"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Complex"),
            args: vec![lit(2.0), lit(-3.0)],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },

        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.plainComplex".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.plainComplex.x.re", vec![]), &env),
        2.0
    );
    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.plainComplex.x.im", vec![]), &env),
        -3.0
    );
}

#[test]
fn test_eval_function_call_selected_complex_output_uses_component_var_ref() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = rumoca_core::Function::new("Pkg.negJ", Default::default());
    f.add_output(rumoca_core::FunctionParam::new(
        "x",
        "Modelica.ComplexMath.Complex",
    ));
    f.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("x"),
        value: rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Box::new(var("j")),
            span: rumoca_core::Span::DUMMY,
        },

        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.negJ".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);
    env.set("j.re", 0.0);
    env.set("j.im", 1.0);

    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.negJ.x.re", vec![]), &env),
        0.0
    );
    assert_eq!(
        eval_expr_or_default::<f64>(&fn_call("Pkg.negJ.x.im", vec![]), &env),
        -1.0
    );
}

#[test]
fn complex_selection_field_matching_uses_top_level_segments() {
    assert_eq!(
        complex_field_selection_from_path("Pkg.powerOfJ.x.re"),
        Some("re")
    );
    assert_eq!(
        complex_field_selection_from_path("Pkg.powerOfJ.x.im"),
        Some("im")
    );
    assert_eq!(
        complex_field_selection_from_path("Pkg.powerOfJ.x.myre"),
        None
    );
    assert_eq!(
        complex_field_selection_from_path("Pkg.powerOfJ.x.reAlias"),
        None
    );
    assert_eq!(
        complex_field_selection_from_path("Pkg.powerOfJ.x[idx.re]"),
        None
    );
    assert_eq!(
        complex_field_selection_from_path("Pkg.powerOfJ.x[idx.re].im"),
        Some("im")
    );
}

#[test]
fn test_eval_function_closure_partial_application_binds_function_input() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut target = rumoca_core::Function::new("Pkg.target", Default::default());
    target.add_input(rumoca_core::FunctionParam::new("u", "Real"));
    target.add_input(rumoca_core::FunctionParam::new("A", "Real"));
    target.add_input(rumoca_core::FunctionParam::new("w", "Real"));
    target.add_output(
        rumoca_core::FunctionParam::new("y", "Real").with_default(binop(
            rumoca_core::OpBinary::Add,
            binop(rumoca_core::OpBinary::Add, var("u"), var("A")),
            var("w"),
        )),
    );
    target.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.target".to_string(), target);

    let mut wrapper = rumoca_core::Function::new("Pkg.wrapper", Default::default());
    wrapper.add_input(rumoca_core::FunctionParam::new(
        "f",
        "Pkg.Interfaces.PartialFunction",
    ));
    wrapper.add_input(rumoca_core::FunctionParam::new("x", "Real"));
    wrapper.add_output(
        rumoca_core::FunctionParam::new("y", "Real")
            .with_default(fn_call("Pkg.wrapper.f", vec![var("x")])),
    );
    wrapper.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.wrapper".to_string(), wrapper);

    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call(
        "Pkg.wrapper",
        vec![fn_call("Pkg.target", vec![lit(2.0), lit(3.0)]), lit(5.0)],
    );
    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 10.0);
}

#[test]
fn test_eval_function_closure_propagates_through_nested_function_arguments() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut target = rumoca_core::Function::new("Pkg.target", Default::default());
    target.add_input(rumoca_core::FunctionParam::new("u", "Real"));
    target.add_input(rumoca_core::FunctionParam::new("A", "Real"));
    target.add_input(rumoca_core::FunctionParam::new("w", "Real"));
    target.add_output(
        rumoca_core::FunctionParam::new("y", "Real").with_default(binop(
            rumoca_core::OpBinary::Add,
            binop(rumoca_core::OpBinary::Add, var("u"), var("A")),
            var("w"),
        )),
    );
    target.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.target".to_string(), target);

    let mut inner = rumoca_core::Function::new("Pkg.outer.inner", Default::default());
    inner.add_input(rumoca_core::FunctionParam::new(
        "f",
        "Pkg.Interfaces.PartialFunction",
    ));
    inner.add_input(rumoca_core::FunctionParam::new("x", "Real"));
    inner.add_output(
        rumoca_core::FunctionParam::new("y", "Real")
            .with_default(fn_call("Pkg.outer.inner.f", vec![var("x")])),
    );
    inner.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.outer.inner".to_string(), inner);

    let mut outer = rumoca_core::Function::new("Pkg.outer", Default::default());
    outer.add_input(rumoca_core::FunctionParam::new(
        "f",
        "Pkg.Interfaces.PartialFunction",
    ));
    outer.add_input(rumoca_core::FunctionParam::new("x", "Real"));
    outer.add_output(
        rumoca_core::FunctionParam::new("y", "Real").with_default(fn_call(
            "Pkg.outer.inner",
            vec![var("Pkg.outer.f"), var("x")],
        )),
    );
    outer.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.outer".to_string(), outer);

    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call(
        "Pkg.outer",
        vec![fn_call("Pkg.target", vec![lit(2.0), lit(3.0)]), lit(5.0)],
    );
    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 10.0);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_varref_argument() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.stateMetric", Default::default());
    f.add_input(rumoca_core::FunctionParam::new("st", "State"));
    f.add_output(
        rumoca_core::FunctionParam::new("y", "Real").with_default(binop(
            rumoca_core::OpBinary::Add,
            var("st.p"),
            var("st.T"),
        )),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.stateMetric".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    env.set("arg.p", 101325.0);
    env.set("arg.T", 350.0);

    let expr = fn_call("Pkg.stateMetric", vec![var("arg")]);
    assert!((eval_expr_or_default::<f64>(&expr, &env) - 101675.0).abs() < 1e-9);
    assert!((eval_expr::<f64>(&expr, &env).expect("strict eval") - 101675.0).abs() < 1e-9);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_start_exprs() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.stateMetric", Default::default());
    f.add_input(rumoca_core::FunctionParam::new("st", "State"));
    f.add_output(
        rumoca_core::FunctionParam::new("y", "Real").with_default(binop(
            rumoca_core::OpBinary::Add,
            var("st.p"),
            var("st.T"),
        )),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.stateMetric".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    let mut starts = IndexMap::new();
    starts.insert("arg.p".to_string(), lit(101325.0));
    starts.insert("arg.T".to_string(), lit(350.0));
    env.start_exprs = std::sync::Arc::new(starts);

    let expr = fn_call("Pkg.stateMetric", vec![var("arg")]);
    assert!((eval_expr::<f64>(&expr, &env).expect("strict eval") - 101675.0).abs() < 1e-9);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_record_function_output() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut state = rumoca_core::Function::new("Pkg.State", Default::default());
    state.is_constructor = true;
    state.add_input(rumoca_core::FunctionParam::new("p", "Real"));
    state.add_input(rumoca_core::FunctionParam::new("T", "Real"));
    funcs.insert("Pkg.State".to_string(), state);

    let mut make_state = rumoca_core::Function::new("Pkg.makeState", Default::default());
    make_state.add_output(
        rumoca_core::FunctionParam::new("out", "State")
            .with_type_class(rumoca_core::ClassType::Record),
    );
    make_state.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("out"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.State"),
            args: vec![
                named_ctor_arg("p", lit(101325.0)),
                named_ctor_arg("T", lit(350.0)),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.makeState".to_string(), make_state);

    let mut metric = rumoca_core::Function::new("Pkg.stateMetric", Default::default());
    metric.add_input(
        rumoca_core::FunctionParam::new("st", "State")
            .with_type_class(rumoca_core::ClassType::Record),
    );
    metric.add_output(
        rumoca_core::FunctionParam::new("y", "Real").with_default(binop(
            rumoca_core::OpBinary::Add,
            var("st.p"),
            var("st.T"),
        )),
    );
    metric.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.stateMetric".to_string(), metric);
    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call("Pkg.stateMetric", vec![fn_call("Pkg.makeState", vec![])]);
    assert!((eval_expr::<f64>(&expr, &env).expect("strict eval") - 101675.0).abs() < 1e-9);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_field_access_argument() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = rumoca_core::Function::new("Pkg.stateMetric", Default::default());
    f.add_input(rumoca_core::FunctionParam::new("st", "State"));
    f.add_output(
        rumoca_core::FunctionParam::new("y", "Real").with_default(binop(
            rumoca_core::OpBinary::Add,
            var("st.p"),
            var("st.T"),
        )),
    );
    f.body = vec![rumoca_core::Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.stateMetric".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    env.set("container.state.p", 200000.0);
    env.set("container.state.T", 500.0);

    let expr = fn_call(
        "Pkg.stateMetric",
        vec![rumoca_core::Expression::FieldAccess {
            base: Box::new(var("container")),
            field: "state".to_string(),
            span: rumoca_core::Span::DUMMY,
        }],
    );
    assert!((eval_expr_or_default::<f64>(&expr, &env) - 200500.0).abs() < 1e-9);
}

#[test]
fn test_eval_function_record_field_array_uses_first_element_in_scalar_context() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut state = rumoca_core::Function::new("Pkg.State", Default::default());
    state.is_constructor = true;
    state.add_input(rumoca_core::FunctionParam::new("p", "Real"));
    state.add_input(rumoca_core::FunctionParam::new("X", "Real").with_dims(vec![2]));
    funcs.insert("Pkg.State".to_string(), state);

    let mut make_state = rumoca_core::Function::new("Pkg.makeState", Default::default());
    make_state.add_output(
        rumoca_core::FunctionParam::new("out", "State")
            .with_type_class(rumoca_core::ClassType::Record),
    );
    make_state.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("out"),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.State"),
            args: vec![
                named_ctor_arg("p", lit(101325.0)),
                named_ctor_arg("X", arr(vec![lit(0.25), lit(0.75)], false)),
            ],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];
    funcs.insert("Pkg.makeState".to_string(), make_state);
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(fn_call("Pkg.makeState", vec![])),
        field: "X".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert!((eval_expr_or_default::<f64>(&expr, &env) - 0.25).abs() < 1e-9);
}

#[test]
fn test_eval_function_record_field_array_uses_named_arg_for_dynamic_shape() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut state = rumoca_core::Function::new("Pkg.State", Default::default());
    state.is_constructor = true;
    state.add_input(rumoca_core::FunctionParam::new("p", "Real"));
    state.add_input(rumoca_core::FunctionParam::new("X", "Real").with_dims(vec![-1]));
    funcs.insert("Pkg.State".to_string(), state);

    let mut set_state = rumoca_core::Function::new("Pkg.setState_pTX", Default::default());
    set_state.add_input(rumoca_core::FunctionParam::new("p", "Real"));
    set_state.add_input(rumoca_core::FunctionParam::new("X", "Real").with_dims(vec![-1]));
    set_state.add_output(
        rumoca_core::FunctionParam::new("state", "State")
            .with_type_class(rumoca_core::ClassType::Record),
    );
    funcs.insert("Pkg.setState".to_string(), set_state);
    env.functions = std::sync::Arc::new(funcs);
    env.dims = Arc::new(IndexMap::from([("X_start".to_string(), vec![2])]));
    set_array_entries(&mut env, "X_start", &[2], &[0.2, 0.8]);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(fn_call(
            "Pkg.setState_pTX",
            vec![
                named_ctor_arg("p", lit(101325.0)),
                named_ctor_arg(
                    "X",
                    rumoca_core::Expression::VarRef {
                        name: Reference::new("X_start"),
                        subscripts: vec![Subscript::generated_expr(Box::new(
                            rumoca_core::Expression::Range {
                                start: Box::new(int_lit(1)),
                                step: None,
                                end: Box::new(int_lit(1)),
                                span: rumoca_core::Span::DUMMY,
                            },
                        ))],
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
            ],
        )),
        field: "X".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_array_values::<f64>(&expr, &env), vec![0.2, 0.8]);
}

#[test]
fn test_eval_set_state_record_field_array_uses_indexed_array_range_named_arg() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut state = rumoca_core::Function::new("Pkg.State", Default::default());
    state.is_constructor = true;
    state.add_input(rumoca_core::FunctionParam::new("X", "Real").with_dims(vec![2]));
    funcs.insert("Pkg.State".to_string(), state);

    let mut set_state = rumoca_core::Function::new("Pkg.setState_pTX", Default::default());
    set_state.add_input(rumoca_core::FunctionParam::new("X", "Real").with_dims(vec![-1]));
    set_state.add_output(
        rumoca_core::FunctionParam::new("state", "State")
            .with_type_class(rumoca_core::ClassType::Record),
    );
    funcs.insert("Pkg.setState_pTX".to_string(), set_state);
    env.functions = std::sync::Arc::new(funcs);

    let substance_names = arr(
        vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("water".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("air".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        false,
    );
    let end = binop(
        rumoca_core::OpBinary::Sub,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![substance_names, int_lit(1)],
            span: rumoca_core::Span::DUMMY,
        },
        int_lit(1),
    );
    let x_slice = rumoca_core::Expression::Index {
        base: Box::new(arr(vec![lit(0.01), lit(0.99)], false)),
        subscripts: vec![Subscript::generated_expr(Box::new(
            rumoca_core::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(end),
                span: rumoca_core::Span::DUMMY,
            },
        ))],
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(fn_call(
            "Pkg.setState_pTX",
            vec![named_ctor_arg("X", x_slice)],
        )),
        field: "X".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_array_values::<f64>(&expr, &env), vec![0.01, 0.99]);
}

#[test]
fn test_eval_function_call_unknown_user_function_returns_nan() {
    let env = VarEnv::<f64>::new();
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("exlin"),
        args: vec![lit(2.0), lit(50.0)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let got = eval_expr_or_default::<f64>(&expr, &env);
    assert!(got.is_nan());
}

#[test]
fn test_eval_function_call_external_stub_falls_back_to_special_handler() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut and_true_stub =
        rumoca_core::Function::new("Modelica.Math.BooleanVectors.andTrue", Default::default());
    and_true_stub.external = Some(rumoca_core::ExternalFunction::default());
    funcs.insert(
        "Modelica.Math.BooleanVectors.andTrue".to_string(),
        and_true_stub,
    );
    env.functions = std::sync::Arc::new(funcs);

    let all_true = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Modelica.Math.BooleanVectors.andTrue"),
        args: vec![arr(vec![bool_lit(true), bool_lit(true)], false)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&all_true, &env), 1.0);

    let one_false = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Modelica.Math.BooleanVectors.andTrue"),
        args: vec![arr(vec![bool_lit(true), bool_lit(false)], false)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_or_default::<f64>(&one_false, &env), 0.0);
}

#[test]
fn test_runtime_special_function_precedence_over_user_body() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut first_true = rumoca_core::Function::new(
        "Modelica.Math.BooleanVectors.firstTrueIndex",
        Default::default(),
    );
    first_true.add_input(rumoca_core::FunctionParam::new("u", "Boolean").with_dims(vec![0]));
    first_true.add_input(rumoca_core::FunctionParam::new("nu", "Integer"));
    first_true.add_output(
        rumoca_core::FunctionParam::new("index", "Integer").with_default(
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: rumoca_core::Span::DUMMY,
            },
        ),
    );
    funcs.insert(
        "Modelica.Math.BooleanVectors.firstTrueIndex".to_string(),
        first_true,
    );
    env.functions = std::sync::Arc::new(funcs);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Modelica.Math.BooleanVectors.firstTrueIndex"),
        args: vec![
            arr(vec![bool_lit(true), bool_lit(true)], false),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr_or_default::<f64>(&expr, &env), 1.0);
}
