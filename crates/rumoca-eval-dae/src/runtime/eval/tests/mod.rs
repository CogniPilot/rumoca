use super::*;
use crate::runtime::dual::Dual;
use indexmap::IndexMap;

type BuiltinFunction = dae::BuiltinFunction;
type Expression = dae::Expression;
type Function = dae::Function;
type FunctionParam = dae::FunctionParam;
type OpBinary = rumoca_ir_core::OpBinary;
type Statement = dae::Statement;
type Subscript = dae::Subscript;
type VarName = dae::VarName;
mod complex_array_projection;
mod pre_seed_regressions;
mod shift_sample_value_form;
mod table_ad_edges;
mod vector_binary_ops;

fn lit(v: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(v))
}

fn int_lit(v: i64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Integer(v))
}

fn bool_lit(v: bool) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Boolean(v))
}

fn dae_lit(v: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(v))
}

fn dae_bool_lit(v: bool) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Boolean(v))
}

fn dae_var(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

fn var(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

fn comp_ref(name: &str) -> dae::ComponentReference {
    dae::ComponentReference {
        local: false,
        parts: vec![dae::ComponentRefPart {
            ident: name.to_string(),
            subs: vec![],
        }],
        def_id: None,
    }
}

fn comp_ref_index(name: &str, index: i64) -> dae::ComponentReference {
    dae::ComponentReference {
        local: false,
        parts: vec![dae::ComponentRefPart {
            ident: name.to_string(),
            subs: vec![dae::Subscript::Index(index)],
        }],
        def_id: None,
    }
}

fn arr(elements: Vec<dae::Expression>, is_matrix: bool) -> dae::Expression {
    dae::Expression::Array {
        elements,
        is_matrix,
    }
}

fn fn_call(name: &str, args: Vec<dae::Expression>) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new(name),
        args,
        is_constructor: false,
    }
}

fn named_ctor_arg(name: &str, value: dae::Expression) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new(format!("__rumoca_named_arg__.{name}")),
        args: vec![value],
        is_constructor: true,
    }
}

fn set_vector_var<T: SimFloat>(env: &mut VarEnv<T>, name: &str, values: &[T]) {
    for (idx, value) in values.iter().enumerate() {
        env.set(&format!("{name}[{}]", idx + 1), *value);
    }
    env.dims = Arc::new(IndexMap::from([(
        name.to_string(),
        vec![values.len() as i64],
    )]));
}

fn simple_table_expr() -> dae::Expression {
    arr(
        vec![
            arr(vec![lit(0.0), lit(10.0)], false),
            arr(vec![lit(2.0), lit(14.0)], false),
        ],
        true,
    )
}

fn columns_expr() -> dae::Expression {
    arr(vec![int_lit(2)], false)
}

fn simple_table_if_expr() -> dae::Expression {
    dae::Expression::If {
        branches: vec![(bool_lit(true), simple_table_expr())],
        else_branch: Box::new(arr(vec![arr(vec![lit(0.0), lit(0.0)], false)], true)),
    }
}

fn eval_table1d_dual(u: Dual, extrapolation: i64) -> Dual {
    let mut env = VarEnv::<Dual>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(extrapolation),
        ],
    );
    let table_id = eval_expr::<Dual>(&constructor, &env).real();
    assert!(table_id > 0.0);
    env.set("table_id", Dual::from_f64(table_id));
    env.set("u", u);
    eval_expr::<Dual>(
        &fn_call(
            "getTable1DValueNoDer",
            vec![var("table_id"), int_lit(1), var("u")],
        ),
        &env,
    )
}

fn eval_timetable_dual(t: Dual, extrapolation: i64) -> Dual {
    let mut env = VarEnv::<Dual>::new();
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            lit(0.0), // startTime
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(extrapolation),
        ],
    );
    let table_id = eval_expr::<Dual>(&constructor, &env).real();
    assert!(table_id > 0.0);
    env.set("table_id", Dual::from_f64(table_id));
    env.set("t", t);
    eval_expr::<Dual>(
        &fn_call(
            "getTimeTableValueNoDer",
            vec![var("table_id"), int_lit(1), var("t"), lit(0.0), lit(0.0)],
        ),
        &env,
    )
}

#[test]
fn test_eval_index_on_matrix_literal() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::Index {
        base: Box::new(simple_table_expr()),
        subscripts: vec![dae::Subscript::Index(2), dae::Subscript::Index(1)],
    };
    let value = eval_expr::<f64>(&expr, &env);
    assert!((value - 2.0).abs() < 1e-12);
}

#[test]
fn test_eval_index_on_flattened_env_array_with_dims() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("A".to_string(), vec![3, 3])]));
    for i in 1..=9 {
        env.set(&format!("A[{i}]"), i as f64);
    }

    let expr = dae::Expression::Index {
        base: Box::new(var("A")),
        subscripts: vec![dae::Subscript::Index(2), dae::Subscript::Index(3)],
    };
    let value = eval_expr::<f64>(&expr, &env);
    assert!((value - 6.0).abs() < 1e-12);
}

#[test]
fn test_eval_array_values_expands_range() {
    let env = VarEnv::<f64>::new();
    let ascending = dae::Expression::Range {
        start: Box::new(int_lit(1)),
        step: None,
        end: Box::new(int_lit(4)),
    };
    let descending = dae::Expression::Range {
        start: Box::new(int_lit(4)),
        step: None,
        end: Box::new(int_lit(1)),
    };

    let up = eval_array_values::<f64>(&ascending, &env);
    let down = eval_array_values::<f64>(&descending, &env);
    assert_eq!(up, vec![1.0, 2.0, 3.0, 4.0]);
    assert_eq!(down, vec![4.0, 3.0, 2.0, 1.0]);
}

fn user_function_with_default_output(name: &str, output_value: f64) -> dae::Function {
    let mut func = dae::Function::new(name, Default::default());
    func.add_output(
        dae::FunctionParam::new("y", "Real")
            .with_default(dae::Expression::Literal(dae::Literal::Real(output_value))),
    );
    // Non-empty body is required for function-body evaluation path.
    func.body = vec![dae::Statement::Empty];
    func
}

fn binop(
    op: rumoca_ir_core::OpBinary,
    lhs: dae::Expression,
    rhs: dae::Expression,
) -> dae::Expression {
    dae::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[test]
fn test_eval_literal_real() {
    assert_eq!(eval_expr::<f64>(&lit(3.125), &VarEnv::new()), 3.125);
}

#[test]
fn test_eval_literal_integer() {
    assert_eq!(eval_expr::<f64>(&int_lit(42), &VarEnv::new()), 42.0);
}

#[test]
fn test_eval_literal_boolean() {
    assert_eq!(eval_expr::<f64>(&bool_lit(true), &VarEnv::new()), 1.0);
    assert_eq!(eval_expr::<f64>(&bool_lit(false), &VarEnv::new()), 0.0);
}

#[test]
fn test_eval_var_ref() {
    let mut env = VarEnv::<f64>::new();
    env.set("x", 2.5);
    assert_eq!(eval_expr::<f64>(&var("x"), &env), 2.5);
}

#[test]
fn test_eval_var_ref_missing() {
    assert_eq!(eval_expr::<f64>(&var("missing"), &VarEnv::new()), 0.0);
}

#[test]
fn test_eval_var_ref_resolves_enum_literal_ordinal() {
    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = Arc::new(IndexMap::from([(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    )]));
    assert_eq!(
        eval_expr::<f64>(
            &var("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
            &env
        ),
        4.0
    );
}

#[test]
fn test_eval_var_ref_resolves_enum_literal_ordinal_without_quotes_in_table() {
    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = Arc::new(IndexMap::from([(
        "Modelica.Electrical.Digital.Interfaces.Logic.1".to_string(),
        4,
    )]));
    assert_eq!(
        eval_expr::<f64>(
            &var("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
            &env
        ),
        4.0
    );
}

#[test]
fn test_eval_var_ref_resolves_enum_literal_ordinal_with_quotes_in_table() {
    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = Arc::new(IndexMap::from([(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    )]));
    assert_eq!(
        eval_expr::<f64>(&var("Modelica.Electrical.Digital.Interfaces.Logic.1"), &env),
        4.0
    );
}

#[test]
fn test_map_var_to_env_size1_array_populates_indexed_alias() {
    let mut env = VarEnv::<f64>::new();
    let mut idx = 0usize;
    let mut arr1 = rumoca_ir_dae::Variable::new(dae::VarName::new("arr1"));
    arr1.dims = vec![1];
    map_var_to_env(&mut env, "arr1", &arr1, &[2.5], &mut idx);
    assert_eq!(idx, 1);
    assert!((env.get("arr1") - 2.5).abs() < 1e-12);
    assert!((env.get("arr1[1]") - 2.5).abs() < 1e-12);
}

#[test]
fn test_build_env_seeds_discrete_start_values() {
    let mut dae = rumoca_ir_dae::Dae::default();
    let mut off = rumoca_ir_dae::Variable::new(dae::VarName::new("off"));
    off.start = Some(dae_bool_lit(true));
    dae.discrete_valued.insert(dae::VarName::new("off"), off);

    let mut z = rumoca_ir_dae::Variable::new(dae::VarName::new("z"));
    z.start = Some(dae_lit(2.5));
    dae.discrete_reals.insert(dae::VarName::new("z"), z);

    let env = build_env(&dae, &[], &[], 0.0);
    assert_eq!(env.get("off"), 1.0);
    assert!((env.get("z") - 2.5).abs() < 1e-12);
}

#[test]
fn test_build_env_discrete_start_forward_ref_re_evaluates_and_preserves_pre_seed() {
    clear_pre_values();

    let mut dae = rumoca_ir_dae::Dae::default();

    // Insert dependent start first to exercise forward-reference handling.
    let mut a = rumoca_ir_dae::Variable::new(dae::VarName::new("a"));
    a.start = Some(dae_var("b"));
    dae.discrete_valued.insert(dae::VarName::new("a"), a);

    let mut b = rumoca_ir_dae::Variable::new(dae::VarName::new("b"));
    b.start = Some(dae_bool_lit(true));
    dae.discrete_valued.insert(dae::VarName::new("b"), b);

    let env = build_env(&dae, &[], &[], 0.0);
    assert_eq!(env.get("b"), 1.0);
    assert_eq!(env.get("a"), 1.0);

    // Pre-seeded values must take precedence over start expressions.
    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("a", 0.0);
    pre_env.set("b", 0.0);
    seed_pre_values_from_env(&pre_env);

    let env_from_pre = build_env(&dae, &[], &[], 1.0);
    assert_eq!(env_from_pre.get("a"), 0.0);
    assert_eq!(env_from_pre.get("b"), 0.0);

    clear_pre_values();
}

#[test]
fn test_eval_binary_add() {
    let expr = binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        lit(2.0),
        lit(3.0),
    );
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 5.0);
}

#[test]
fn test_eval_unary_minus() {
    let expr = dae::Expression::Unary {
        op: rumoca_ir_core::OpUnary::Minus(Default::default()),
        rhs: Box::new(lit(5.0)),
    };
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), -5.0);
}

#[test]
fn test_eval_unary_not() {
    let expr = dae::Expression::Unary {
        op: rumoca_ir_core::OpUnary::Not(Default::default()),
        rhs: Box::new(bool_lit(true)),
    };
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 0.0);
}

#[test]
fn test_eval_builtin_sin() {
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sin,
        args: vec![lit(0.0)],
    };
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 0.0);
}

#[test]
fn test_eval_builtin_cos() {
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Cos,
        args: vec![lit(0.0)],
    };
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 1.0);
}

#[test]
fn test_eval_builtin_sqrt() {
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sqrt,
        args: vec![lit(9.0)],
    };
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 3.0);
}

#[test]
fn test_eval_builtin_exp() {
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Exp,
        args: vec![lit(0.0)],
    };
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 1.0);
}

#[test]
fn test_eval_builtin_abs() {
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Abs,
        args: vec![lit(-3.0)],
    };
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 3.0);
}

#[test]
fn test_eval_builtin_min_max() {
    let min_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Min,
        args: vec![lit(3.0), lit(5.0)],
    };
    assert_eq!(eval_expr::<f64>(&min_expr, &VarEnv::new()), 3.0);

    let max_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Max,
        args: vec![lit(3.0), lit(5.0)],
    };
    assert_eq!(eval_expr::<f64>(&max_expr, &VarEnv::new()), 5.0);
}

#[test]
fn test_eval_builtin_one_arg_array_reductions() {
    let mut env = VarEnv::<f64>::new();
    set_vector_var(&mut env, "x", &[-2.0, 3.0, 4.0]);

    let min_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Min,
        args: vec![var("x")],
    };
    assert_eq!(eval_expr::<f64>(&min_expr, &env), -2.0);

    let max_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Max,
        args: vec![var("x")],
    };
    assert_eq!(eval_expr::<f64>(&max_expr, &env), 4.0);

    let sum_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sum,
        args: vec![var("x")],
    };
    assert_eq!(eval_expr::<f64>(&sum_expr, &env), 5.0);

    let product_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Product,
        args: vec![var("x")],
    };
    assert_eq!(eval_expr::<f64>(&product_expr, &env), -24.0);
}

#[test]
fn test_eval_builtin_max_abs_single_arg_array_reduction() {
    let mut env = VarEnv::<f64>::new();
    set_vector_var(&mut env, "aux", &[-2.0, 0.25, 3.5, -1.0]);

    let max_abs_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Max,
        args: vec![dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Abs,
            args: vec![var("aux")],
        }],
    };
    assert_eq!(eval_expr::<f64>(&max_abs_expr, &env), 3.5);
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

    let sum_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sum,
        args: vec![var("x")],
    };
    let y = eval_expr::<Dual>(&sum_expr, &env);
    assert!((y.re - 4.0).abs() < 1e-12);
    assert!((y.du - 1.5).abs() < 1e-12);
}

#[test]
fn test_eval_if_true() {
    let expr = dae::Expression::If {
        branches: vec![(bool_lit(true), lit(1.0))],
        else_branch: Box::new(lit(2.0)),
    };
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 1.0);
}

#[test]
fn test_eval_if_false() {
    let expr = dae::Expression::If {
        branches: vec![(bool_lit(false), lit(1.0))],
        else_branch: Box::new(lit(2.0)),
    };
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 2.0);
}

#[test]
fn test_eval_comparison() {
    let lt = binop(
        rumoca_ir_core::OpBinary::Lt(Default::default()),
        lit(1.0),
        lit(2.0),
    );
    assert_eq!(eval_expr::<f64>(&lt, &VarEnv::new()), 1.0);

    let gt = binop(
        rumoca_ir_core::OpBinary::Gt(Default::default()),
        lit(1.0),
        lit(2.0),
    );
    assert_eq!(eval_expr::<f64>(&gt, &VarEnv::new()), 0.0);
}

#[test]
fn test_eval_der_lookup() {
    let mut env = VarEnv::<f64>::new();
    env.set("der(x)", 5.0);

    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![var("x")],
    };
    assert_eq!(eval_expr::<f64>(&expr, &env), 5.0);
}

#[test]
fn test_eval_complex_expression() {
    let mut env = VarEnv::<f64>::new();
    env.set("x", 3.0);
    env.set("y", std::f64::consts::FRAC_PI_2);

    let expr = binop(
        rumoca_ir_core::OpBinary::Mul(Default::default()),
        binop(
            rumoca_ir_core::OpBinary::Add(Default::default()),
            lit(2.0),
            var("x"),
        ),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sin,
            args: vec![var("y")],
        },
    );
    let result = eval_expr::<f64>(&expr, &env);
    assert!((result - 5.0).abs() < 1e-10);
}

#[test]
fn test_eval_field_access_on_var_ref_uses_flat_field_name() {
    let mut env = VarEnv::<f64>::new();
    env.set("z.re", 2.5);
    let expr = dae::Expression::FieldAccess {
        base: Box::new(var("z")),
        field: "re".to_string(),
    };
    assert_eq!(eval_expr::<f64>(&expr, &env), 2.5);
}

#[test]
fn test_eval_field_access_on_constructor_complex_components() {
    let env = VarEnv::<f64>::new();
    let ctor = dae::Expression::FunctionCall {
        name: dae::VarName::new("Complex"),
        args: vec![lit(3.0), lit(4.0)],
        is_constructor: true,
    };
    let re_expr = dae::Expression::FieldAccess {
        base: Box::new(ctor.clone()),
        field: "re".to_string(),
    };
    let im_expr = dae::Expression::FieldAccess {
        base: Box::new(ctor),
        field: "im".to_string(),
    };
    assert_eq!(eval_expr::<f64>(&re_expr, &env), 3.0);
    assert_eq!(eval_expr::<f64>(&im_expr, &env), 4.0);
}

#[test]
fn test_eval_constructor_call_scalar_fallback_uses_first_argument() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Complex"),
        args: vec![lit(9.0), lit(1.0)],
        is_constructor: true,
    };
    assert_eq!(eval_expr::<f64>(&expr, &env), 9.0);
}

#[test]
fn test_eval_field_access_constructor_named_args_bind_by_name() {
    let mut ctor = dae::Function::new(
        "Modelica.Electrical.Machines.Losses.CoreParameters",
        Default::default(),
    );
    ctor.add_input(dae::FunctionParam::new("m", "Integer"));
    ctor.add_input(dae::FunctionParam::new("PRef", "Real").with_default(lit(0.0)));
    ctor.add_input(dae::FunctionParam::new("VRef", "Real"));
    ctor.add_input(dae::FunctionParam::new("wRef", "Real"));
    ctor.add_input(
        dae::FunctionParam::new("GcRef", "Real").with_default(dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Le(Default::default()),
                    lhs: Box::new(var("PRef")),
                    rhs: Box::new(lit(0.0)),
                },
                lit(0.0),
            )],
            else_branch: Box::new(dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Div(Default::default()),
                lhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Div(Default::default()),
                    lhs: Box::new(var("PRef")),
                    rhs: Box::new(dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Mul(Default::default()),
                        lhs: Box::new(var("VRef")),
                        rhs: Box::new(var("VRef")),
                    }),
                }),
                rhs: Box::new(var("m")),
            }),
        }),
    );

    let mut env = VarEnv::<f64>::new();
    env.functions = Arc::new(IndexMap::from([(
        "Modelica.Electrical.Machines.Losses.CoreParameters".to_string(),
        ctor,
    )]));
    env.set("m", 3.0);
    env.set("wRef", 314.0);

    let ctor_call = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Electrical.Machines.Losses.CoreParameters"),
        args: vec![
            named_ctor_arg("PRef", lit(410.0)),
            named_ctor_arg("VRef", lit(387.9)),
        ],
        is_constructor: true,
    };

    let p_ref = dae::Expression::FieldAccess {
        base: Box::new(ctor_call.clone()),
        field: "PRef".to_string(),
    };
    let v_ref = dae::Expression::FieldAccess {
        base: Box::new(ctor_call.clone()),
        field: "VRef".to_string(),
    };
    let gc_ref = dae::Expression::FieldAccess {
        base: Box::new(ctor_call),
        field: "GcRef".to_string(),
    };

    let p_ref_value = eval_expr::<f64>(&p_ref, &env);
    let v_ref_value = eval_expr::<f64>(&v_ref, &env);
    let gc_ref_value = eval_expr::<f64>(&gc_ref, &env);

    assert!((p_ref_value - 410.0).abs() < 1e-12);
    assert!((v_ref_value - 387.9).abs() < 1e-12);
    assert!(gc_ref_value.is_finite() && gc_ref_value > 0.0);
}

#[test]
fn test_eval_dual_x_squared() {
    // f(x) = x*x at x=3 → f=9, f'=6
    let mut env = VarEnv::<Dual>::new();
    env.set("x", Dual::new(3.0, 1.0));

    let expr = binop(
        rumoca_ir_core::OpBinary::Mul(Default::default()),
        var("x"),
        var("x"),
    );
    let result = eval_expr::<Dual>(&expr, &env);
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
    let mut var = dae::Variable::new(dae::VarName::new("Modelica.Constants.pi"));
    var.start = Some(dae::Expression::Literal(dae::Literal::Real(3.0)));
    dae.constants
        .insert(dae::VarName::new("Modelica.Constants.pi"), var);

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

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Pkg.localFn"),
        args: vec![],
        is_constructor: false,
    };
    assert!(eval_expr::<f64>(&expr, &env).is_nan());
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

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("localFn"),
        args: vec![],
        is_constructor: false,
    };
    assert!(eval_expr::<f64>(&expr, &env).is_nan());
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

    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("Pkg.localFn"),
        args: vec![],
        is_constructor: false,
    };
    assert_eq!(eval_expr::<f64>(&expr, &env), 11.0);
}

#[test]
fn test_eval_function_call_named_args_bind_by_name() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = dae::Function::new("Pkg.affine", Default::default());
    f.add_input(dae::FunctionParam::new("u", "Real"));
    f.add_input(dae::FunctionParam::new("A", "Real"));
    f.add_input(dae::FunctionParam::new("w", "Real"));
    f.add_output(dae::FunctionParam::new("y", "Real").with_default(binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        binop(
            rumoca_ir_core::OpBinary::Add(Default::default()),
            var("u"),
            var("A"),
        ),
        var("w"),
    )));
    f.body = vec![dae::Statement::Empty];
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
    assert_eq!(eval_expr::<f64>(&expr, &env), 10.0);
}

#[test]
fn test_eval_function_call_defaults_can_reference_prior_inputs() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = dae::Function::new("Pkg.defaults", Default::default());
    f.add_input(dae::FunctionParam::new("a", "Real"));
    f.add_input(dae::FunctionParam::new("b", "Real").with_default(binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        var("a"),
        lit(1.0),
    )));
    f.add_input(dae::FunctionParam::new("c", "Real").with_default(binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        var("b"),
        lit(1.0),
    )));
    f.add_output(dae::FunctionParam::new("y", "Real").with_default(var("c")));
    f.body = vec![dae::Statement::Empty];
    funcs.insert("Pkg.defaults".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call("Pkg.defaults", vec![named_ctor_arg("a", lit(2.0))]);
    assert_eq!(eval_expr::<f64>(&expr, &env), 4.0);
}

#[test]
fn test_eval_function_call_projected_outputs_and_indexed_assignments() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut random_like = dae::Function::new("Pkg.randomLike", Default::default());
    random_like.add_input(dae::FunctionParam::new("seedIn", "Integer").with_dims(vec![3]));
    random_like.add_output(dae::FunctionParam::new("x", "Real"));
    random_like.add_output(dae::FunctionParam::new("seedOut", "Integer").with_dims(vec![3]));
    random_like.body = vec![
        dae::Statement::Assignment {
            comp: comp_ref("x"),
            value: lit(0.25),
        },
        dae::Statement::Assignment {
            comp: comp_ref_index("seedOut", 1),
            value: int_lit(11),
        },
        dae::Statement::Assignment {
            comp: comp_ref_index("seedOut", 2),
            value: int_lit(22),
        },
        dae::Statement::Assignment {
            comp: comp_ref_index("seedOut", 3),
            value: int_lit(33),
        },
    ];
    funcs.insert("Pkg.randomLike".to_string(), random_like);
    env.functions = std::sync::Arc::new(funcs);

    let args = vec![arr(vec![int_lit(1), int_lit(2), int_lit(3)], false)];
    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.randomLike", args.clone()), &env),
        0.25
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.randomLike.seedOut[2]", args), &env),
        22.0
    );
}

#[test]
fn test_eval_function_call_projected_complex_output_components() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = dae::Function::new("Pkg.powerOfJ", Default::default());
    f.add_output(dae::FunctionParam::new("x", "Modelica.ComplexMath.Complex"));
    f.body = vec![dae::Statement::Assignment {
        comp: comp_ref("x"),
        value: dae::Expression::FunctionCall {
            name: dae::VarName::new("Complex"),
            args: vec![lit(2.0), lit(-3.0)],
            is_constructor: true,
        },
    }];
    funcs.insert("Pkg.powerOfJ".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.powerOfJ.x.re", vec![]), &env),
        2.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.powerOfJ.x.im", vec![]), &env),
        -3.0
    );
}

#[test]
fn test_eval_function_call_projected_complex_output_from_single_arg_constructor() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = dae::Function::new("Pkg.singleArgComplex", Default::default());
    f.add_output(dae::FunctionParam::new("x", "Modelica.ComplexMath.Complex"));
    f.body = vec![dae::Statement::Assignment {
        comp: comp_ref("x"),
        value: dae::Expression::FunctionCall {
            name: dae::VarName::new("Complex"),
            args: vec![lit(1.0)],
            is_constructor: true,
        },
    }];
    funcs.insert("Pkg.singleArgComplex".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.singleArgComplex.x.re", vec![]), &env),
        1.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.singleArgComplex.x.im", vec![]), &env),
        0.0
    );
}

#[test]
fn test_eval_function_call_projected_complex_output_uses_component_var_ref() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut f = dae::Function::new("Pkg.negJ", Default::default());
    f.add_output(dae::FunctionParam::new("x", "Modelica.ComplexMath.Complex"));
    f.body = vec![dae::Statement::Assignment {
        comp: comp_ref("x"),
        value: dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(Default::default()),
            rhs: Box::new(var("j")),
        },
    }];
    funcs.insert("Pkg.negJ".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);
    env.set("j.re", 0.0);
    env.set("j.im", 1.0);

    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.negJ.x.re", vec![]), &env),
        0.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Pkg.negJ.x.im", vec![]), &env),
        -1.0
    );
}

#[test]
fn test_eval_function_closure_partial_application_binds_function_input() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut target = dae::Function::new("Pkg.target", Default::default());
    target.add_input(dae::FunctionParam::new("u", "Real"));
    target.add_input(dae::FunctionParam::new("A", "Real"));
    target.add_input(dae::FunctionParam::new("w", "Real"));
    target.add_output(dae::FunctionParam::new("y", "Real").with_default(binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        binop(
            rumoca_ir_core::OpBinary::Add(Default::default()),
            var("u"),
            var("A"),
        ),
        var("w"),
    )));
    target.body = vec![dae::Statement::Empty];
    funcs.insert("Pkg.target".to_string(), target);

    let mut wrapper = dae::Function::new("Pkg.wrapper", Default::default());
    wrapper.add_input(dae::FunctionParam::new(
        "f",
        "Pkg.Interfaces.PartialFunction",
    ));
    wrapper.add_input(dae::FunctionParam::new("x", "Real"));
    wrapper.add_output(
        dae::FunctionParam::new("y", "Real").with_default(fn_call("Pkg.wrapper.f", vec![var("x")])),
    );
    wrapper.body = vec![dae::Statement::Empty];
    funcs.insert("Pkg.wrapper".to_string(), wrapper);

    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call(
        "Pkg.wrapper",
        vec![fn_call("Pkg.target", vec![lit(2.0), lit(3.0)]), lit(5.0)],
    );
    assert_eq!(eval_expr::<f64>(&expr, &env), 10.0);
}

#[test]
fn test_eval_function_closure_propagates_through_nested_function_arguments() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();

    let mut target = dae::Function::new("Pkg.target", Default::default());
    target.add_input(dae::FunctionParam::new("u", "Real"));
    target.add_input(dae::FunctionParam::new("A", "Real"));
    target.add_input(dae::FunctionParam::new("w", "Real"));
    target.add_output(dae::FunctionParam::new("y", "Real").with_default(binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        binop(
            rumoca_ir_core::OpBinary::Add(Default::default()),
            var("u"),
            var("A"),
        ),
        var("w"),
    )));
    target.body = vec![dae::Statement::Empty];
    funcs.insert("Pkg.target".to_string(), target);

    let mut inner = dae::Function::new("Pkg.outer.inner", Default::default());
    inner.add_input(dae::FunctionParam::new(
        "f",
        "Pkg.Interfaces.PartialFunction",
    ));
    inner.add_input(dae::FunctionParam::new("x", "Real"));
    inner.add_output(
        dae::FunctionParam::new("y", "Real")
            .with_default(fn_call("Pkg.outer.inner.f", vec![var("x")])),
    );
    inner.body = vec![dae::Statement::Empty];
    funcs.insert("Pkg.outer.inner".to_string(), inner);

    let mut outer = dae::Function::new("Pkg.outer", Default::default());
    outer.add_input(dae::FunctionParam::new(
        "f",
        "Pkg.Interfaces.PartialFunction",
    ));
    outer.add_input(dae::FunctionParam::new("x", "Real"));
    outer.add_output(dae::FunctionParam::new("y", "Real").with_default(fn_call(
        "Pkg.outer.inner",
        vec![var("Pkg.outer.f"), var("x")],
    )));
    outer.body = vec![dae::Statement::Empty];
    funcs.insert("Pkg.outer".to_string(), outer);

    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call(
        "Pkg.outer",
        vec![fn_call("Pkg.target", vec![lit(2.0), lit(3.0)]), lit(5.0)],
    );
    assert_eq!(eval_expr::<f64>(&expr, &env), 10.0);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_varref_argument() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = dae::Function::new("Pkg.stateMetric", Default::default());
    f.add_input(dae::FunctionParam::new("st", "State"));
    f.add_output(dae::FunctionParam::new("y", "Real").with_default(binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        var("st.p"),
        var("st.T"),
    )));
    f.body = vec![dae::Statement::Empty];
    funcs.insert("Pkg.stateMetric".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    env.set("arg.p", 101325.0);
    env.set("arg.T", 350.0);

    let expr = fn_call("Pkg.stateMetric", vec![var("arg")]);
    assert!((eval_expr::<f64>(&expr, &env) - 101675.0).abs() < 1e-9);
}

#[test]
fn test_eval_user_function_binds_record_input_fields_from_field_access_argument() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut f = dae::Function::new("Pkg.stateMetric", Default::default());
    f.add_input(dae::FunctionParam::new("st", "State"));
    f.add_output(dae::FunctionParam::new("y", "Real").with_default(binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        var("st.p"),
        var("st.T"),
    )));
    f.body = vec![dae::Statement::Empty];
    funcs.insert("Pkg.stateMetric".to_string(), f);
    env.functions = std::sync::Arc::new(funcs);

    env.set("container.state.p", 200000.0);
    env.set("container.state.T", 500.0);

    let expr = fn_call(
        "Pkg.stateMetric",
        vec![dae::Expression::FieldAccess {
            base: Box::new(var("container")),
            field: "state".to_string(),
        }],
    );
    assert!((eval_expr::<f64>(&expr, &env) - 200500.0).abs() < 1e-9);
}

#[test]
fn test_eval_function_call_unknown_user_function_returns_nan() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("exlin"),
        args: vec![lit(2.0), lit(50.0)],
        is_constructor: false,
    };
    let got = eval_expr::<f64>(&expr, &env);
    assert!(got.is_nan());
}

#[test]
fn test_eval_function_call_external_stub_falls_back_to_special_handler() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    let mut and_true_stub =
        dae::Function::new("Modelica.Math.BooleanVectors.andTrue", Default::default());
    and_true_stub.external = Some(dae::ExternalFunction::default());
    funcs.insert(
        "Modelica.Math.BooleanVectors.andTrue".to_string(),
        and_true_stub,
    );
    env.functions = std::sync::Arc::new(funcs);

    let all_true = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Math.BooleanVectors.andTrue"),
        args: vec![arr(vec![bool_lit(true), bool_lit(true)], false)],
        is_constructor: false,
    };
    assert_eq!(eval_expr::<f64>(&all_true, &env), 1.0);

    let one_false = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Math.BooleanVectors.andTrue"),
        args: vec![arr(vec![bool_lit(true), bool_lit(false)], false)],
        is_constructor: false,
    };
    assert_eq!(eval_expr::<f64>(&one_false, &env), 0.0);
}

#[test]
fn test_eval_clock_special_functions_are_finite() {
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.0);

    let clock_expr = fn_call("Clock", vec![lit(0.02)]);
    let sub_expr = fn_call("subSample", vec![clock_expr.clone(), lit(2.0)]);
    let super_expr = fn_call("superSample", vec![clock_expr.clone(), lit(2.0)]);
    let shift_expr = fn_call(
        "shiftSample",
        vec![clock_expr.clone(), lit(1.0), lit(100.0)],
    );
    let back_expr = fn_call("backSample", vec![clock_expr.clone(), lit(1.0), lit(100.0)]);
    let hold_expr = fn_call("hold", vec![lit(7.0)]);
    let previous_expr = fn_call("previous", vec![lit(3.0)]);
    let interval_expr = fn_call("interval", vec![clock_expr.clone()]);
    let first_tick_expr = fn_call("firstTick", vec![]);

    for expr in [
        clock_expr,
        sub_expr,
        super_expr,
        shift_expr,
        back_expr,
        hold_expr,
        previous_expr,
        interval_expr,
        first_tick_expr,
    ] {
        let v = eval_expr::<f64>(&expr, &env);
        assert!(v.is_finite(), "clock special function returned non-finite");
    }
}

#[test]
fn test_eval_interval_for_clocked_var_uses_env_clock_interval_metadata() {
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.0);
    env.clock_intervals = std::sync::Arc::new(IndexMap::from([("pulse.simTime".to_string(), 0.1)]));

    let interval_expr = fn_call("interval", vec![var("pulse.simTime")]);
    let value = eval_expr::<f64>(&interval_expr, &env);
    assert!(
        (value - 0.1).abs() <= 1e-12,
        "expected interval(pulse.simTime)=0.1, got {value}"
    );
}

#[test]
fn test_eval_subsample_counter_clock_uses_factor_resolution_ratio() {
    let mut env = VarEnv::<f64>::new();
    env.set("factor", 20.0);
    env.set("resolutionFactor", 1000.0);

    let sub_expr = fn_call(
        "subSample",
        vec![
            fn_call("Clock", vec![var("factor")]),
            var("resolutionFactor"),
        ],
    );

    for (time, expected_tick) in [
        (0.0, true),
        (0.01, false),
        (0.02, true),
        (0.03, false),
        (0.04, true),
    ] {
        env.set("time", time);
        let value = eval_expr::<f64>(&sub_expr, &env);
        assert_eq!(
            value > 0.5,
            expected_tick,
            "subSample(Clock(factor), resolutionFactor) tick mismatch at t={time}: value={value}"
        );
    }
}

#[test]
fn test_eval_builtin_sample_with_clock_returns_sampled_value_not_event_tick() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    env.set("x", 10.0);
    env.set("time", 0.0);

    let sample_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![var("x"), fn_call("Clock", vec![lit(0.5)])],
    };

    // Tick at t=0.0 captures x=10.
    assert_eq!(eval_expr::<f64>(&sample_expr, &env), 10.0);

    // Clocked-value sample form should evaluate the sampled expression, not the
    // boolean event indicator from sample(start, interval).
    env.set("x", 20.0);
    env.set("time", 0.25);
    assert_eq!(eval_expr::<f64>(&sample_expr, &env), 20.0);

    // Also at event instants, return sampled value.
    env.set("time", 0.5);
    assert_eq!(eval_expr::<f64>(&sample_expr, &env), 20.0);
}

#[test]
fn test_eval_builtin_sample_start_interval_keeps_event_boolean_semantics() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    let sample_event_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![lit(0.0), lit(0.5)],
    };

    env.set("time", 0.0);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 1.0);

    env.set("time", 0.25);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 0.0);

    env.set("time", 0.5);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 1.0);
}

#[test]
fn test_eval_builtin_sample_internal_three_arg_form_uses_start_and_interval() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    let sample_event_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![lit(3.0), lit(0.5), lit(0.1)],
    };

    env.set("time", 0.0);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 0.0);

    env.set("time", 0.5);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 1.0);

    env.set("time", 0.6);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 1.0);

    env.set("time", 0.65);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 0.0);
}

#[test]
fn test_eval_stream_special_functions_passthrough() {
    let mut env = VarEnv::<f64>::new();
    env.set("port.h_outflow", 123.0);

    let actual_stream = fn_call("actualStream", vec![var("port.h_outflow")]);
    let in_stream = fn_call("inStream", vec![var("port.h_outflow")]);

    assert_eq!(eval_expr::<f64>(&actual_stream, &env), 123.0);
    assert_eq!(eval_expr::<f64>(&in_stream, &env), 123.0);
}

#[test]
fn test_eval_state_accessor_special_functions() {
    let mut env = VarEnv::<f64>::new();
    env.set("state.T", 312.5);
    env.set("state.p", 101325.0);
    env.set("state.d", 998.2);
    env.set("state.h", 2.6e5);
    env.set("state.u", 2.4e5);
    env.set("state.s", 900.0);
    env.set("state_a.T", 315.0);
    env.set("states[1].T", 320.0);
    env.set("medium.state.T", 333.0);
    env.set("media[2].state.T", 346.0);

    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.temperature", vec![var("state")]), &env),
        312.5
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.pressure", vec![var("state")]), &env),
        101325.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.density", vec![var("state")]), &env),
        998.2
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call("Medium.specificEnthalpy", vec![var("state")]),
            &env
        ),
        2.6e5
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call("Medium.specificInternalEnergy", vec![var("state")]),
            &env
        ),
        2.4e5
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.specificEntropy", vec![var("state")]), &env),
        900.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.temperature", vec![var("state_a")]), &env),
        315.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.temperature", vec![var("states")]), &env),
        320.0
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call(
                "Medium.temperature",
                vec![dae::Expression::FieldAccess {
                    base: Box::new(var("medium")),
                    field: "state".to_string(),
                }],
            ),
            &env
        ),
        333.0
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call(
                "Medium.temperature",
                vec![dae::Expression::FieldAccess {
                    base: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("media"),
                        subscripts: vec![dae::Subscript::Index(2)],
                    }),
                    field: "state".to_string(),
                }],
            ),
            &env
        ),
        346.0
    );

    let h_from_phx = eval_expr::<f64>(
        &fn_call(
            "Medium.specificEnthalpy",
            vec![fn_call(
                "Medium.setState_phX",
                vec![lit(2.0e5), lit(4.2e5), arr(vec![lit(1.0)], false)],
            )],
        ),
        &env,
    );
    assert!((h_from_phx - 4.2e5).abs() < 1e-9);

    let t_from_ptx = eval_expr::<f64>(
        &fn_call(
            "Medium.temperature",
            vec![fn_call(
                "Medium.setState_pTX",
                vec![lit(1.5e5), lit(333.0), arr(vec![lit(1.0)], false)],
            )],
        ),
        &env,
    );
    assert!((t_from_ptx - 333.0).abs() < 1e-9);
}

#[test]
fn test_eval_state_accessor_temperature_setstate_phx_uses_user_helper() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    funcs.insert(
        "Medium.temperature_phX".to_string(),
        user_function_with_default_output("Medium.temperature_phX", 347.5),
    );
    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call(
        "Medium.temperature",
        vec![fn_call(
            "Medium.setState_phX",
            vec![lit(1.2e5), lit(4.0e5), arr(vec![lit(1.0)], false)],
        )],
    );
    assert!((eval_expr::<f64>(&expr, &env) - 347.5).abs() < 1e-9);
}

#[test]
fn test_eval_builtin_sample_behaviour() {
    let mut env = VarEnv::<f64>::new();
    env.set("u", 3.5);
    env.set("time", 0.0);

    let sampled_value = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![var("u")],
    };
    assert!((eval_expr::<f64>(&sampled_value, &env) - 3.5).abs() < 1e-12);

    let sample_tick = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![lit(0.0), lit(0.5)],
    };
    assert_eq!(eval_expr::<f64>(&sample_tick, &env), 1.0);
    env.set("time", 0.25);
    assert_eq!(eval_expr::<f64>(&sample_tick, &env), 0.0);
}

#[test]
fn test_eval_function_call_builtin_case_insensitive_alias() {
    let env = VarEnv::<f64>::new();
    let expr = fn_call("Integer", vec![lit(1.8)]);
    assert_eq!(eval_expr::<f64>(&expr, &env), 1.0);
}

#[test]
fn test_eval_special_distribution_density_overloads() {
    let mut env = VarEnv::<f64>::new();

    env.set("u", 0.0);
    env.set("u_min", -2.0);
    env.set("u_max", 2.0);
    let uniform = eval_expr::<f64>(
        &fn_call("distribution", vec![var("u"), var("u_min"), var("u_max")]),
        &env,
    );
    assert!((uniform - 0.25).abs() < 1e-12);

    env.set("mu", 0.0);
    env.set("sigma", 2.0);
    let normal = eval_expr::<f64>(
        &fn_call("distribution", vec![var("u"), var("mu"), var("sigma")]),
        &env,
    );
    assert!((normal - 0.199_471_140_200_716_35).abs() < 1e-12);

    env.set("lambda", 3.0);
    env.set("k", 1.5);
    env.set("u", 1.0);
    let weibull = eval_expr::<f64>(
        &fn_call("distribution", vec![var("u"), var("lambda"), var("k")]),
        &env,
    );
    assert!(weibull.is_finite() && weibull > 0.0);
}

#[test]
fn test_eval_builtin_cat_array_values() {
    let env = VarEnv::<f64>::new();
    let cat_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Cat,
        args: vec![
            int_lit(1),
            arr(vec![lit(1.0), lit(2.0)], false),
            arr(vec![lit(3.0)], false),
        ],
    };

    let scalar = eval_expr::<f64>(&cat_expr, &env);
    assert!((scalar - 1.0).abs() < 1e-12);

    let values = eval_array_like_f64_values(&cat_expr, &env);
    assert_eq!(values, vec![1.0, 2.0, 3.0]);
}

#[test]
fn test_table1d_lookup_dual_linear_ad_slope() {
    let mut env = VarEnv::<Dual>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(1), // HoldLastPoint
        ],
    );
    let table_id = eval_expr::<Dual>(&constructor, &env).real();
    assert!(table_id > 0.0);
    env.set("table_id", Dual::from_f64(table_id));
    env.set("u", Dual::new(1.0, 1.0));

    let lookup = fn_call(
        "getTable1DValueNoDer",
        vec![var("table_id"), int_lit(1), var("u")],
    );
    let y = eval_expr::<Dual>(&lookup, &env);
    assert!((y.re - 12.0).abs() < 1e-12);
    assert!((y.du - 2.0).abs() < 1e-12);
}

#[test]
fn test_table1d_hold_extrapolation_clamps_ad_slope_to_zero() {
    let mut env = VarEnv::<Dual>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(1), // HoldLastPoint
        ],
    );
    let table_id = eval_expr::<Dual>(&constructor, &env).real();
    assert!(table_id > 0.0);
    env.set("table_id", Dual::from_f64(table_id));
    env.set("u", Dual::new(5.0, 1.0));

    let lookup = fn_call(
        "getTable1DValueNoDer",
        vec![var("table_id"), int_lit(1), var("u")],
    );
    let y = eval_expr::<Dual>(&lookup, &env);
    assert!((y.re - 14.0).abs() < 1e-12);
    assert!(y.du.abs() < 1e-12);
}

#[test]
fn test_table1d_constructor_uses_start_expr_fallback_for_dynamic_dims() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("tbl".to_string(), vec![0, 2])]));
    env.start_exprs = Arc::new(IndexMap::from([("tbl".to_string(), simple_table_expr())]));

    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            var("tbl"),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);

    env.set("table_id", table_id);
    env.set("u", 1.0);
    let lookup = fn_call(
        "getTable1DValueNoDer",
        vec![var("table_id"), int_lit(1), var("u")],
    );
    let y = eval_expr::<f64>(&lookup, &env);
    assert!((y - 12.0).abs() < 1e-12);
}

#[test]
fn test_time_table_bounds_and_lookup() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            lit(0.0), // startTime
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(1), // HoldLastPoint
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);
    env.set("table_id", table_id);

    let t_min = eval_expr::<f64>(&fn_call("getTimeTableTmin", vec![var("table_id")]), &env);
    let t_max = eval_expr::<f64>(&fn_call("getTimeTableTmax", vec![var("table_id")]), &env);
    assert!((t_min - 0.0).abs() < 1e-12);
    assert!((t_max - 2.0).abs() < 1e-12);

    let y = eval_expr::<f64>(
        &fn_call(
            "getTimeTableValueNoDer",
            vec![var("table_id"), int_lit(1), lit(1.0)],
        ),
        &env,
    );
    assert!((y - 12.0).abs() < 1e-12);
}

#[test]
fn test_time_table_constructor_uses_if_matrix_start_fallback() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("table_dyn".to_string(), vec![0, 2])]));
    env.start_exprs = Arc::new(IndexMap::from([(
        "table_dyn".to_string(),
        simple_table_if_expr(),
    )]));

    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            var("table_dyn"),
            lit(0.0), // startTime
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(1), // HoldLastPoint
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);

    env.set("table_id", table_id);
    let y = eval_expr::<f64>(
        &fn_call(
            "getTimeTableValueNoDer",
            vec![var("table_id"), int_lit(1), lit(1.0)],
        ),
        &env,
    );
    assert!((y - 12.0).abs() < 1e-12);
}

#[test]
fn test_eval_array_values_handles_array_comprehension() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![dae::ComprehensionIndex {
            name: "i".to_string(),
            range: dae::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(int_lit(4)),
            },
        }],
        filter: None,
    };
    let values = eval_array_values::<f64>(&expr, &env);
    assert_eq!(values, vec![1.0, 2.0, 3.0, 4.0]);
}

#[test]
fn test_time_table_next_event_edges() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            lit(0.0),
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(1), // HoldLastPoint
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);
    env.set("table_id", table_id);

    let before_start = eval_expr::<f64>(
        &fn_call("getNextTimeEvent", vec![var("table_id"), lit(-0.25)]),
        &env,
    );
    assert!((before_start - 0.0).abs() < 1e-12);

    let at_start = eval_expr::<f64>(
        &fn_call("getNextTimeEvent", vec![var("table_id"), lit(0.0)]),
        &env,
    );
    assert!((at_start - 2.0).abs() < 1e-12);

    let at_end = eval_expr::<f64>(
        &fn_call("getNextTimeEvent", vec![var("table_id"), lit(2.0)]),
        &env,
    );
    assert!(at_end.is_infinite() && at_end.is_sign_positive());
}

#[test]
fn test_time_table_next_event_periodic_wrap() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            lit(0.0),
            columns_expr(),
            int_lit(1), // LinearSegments
            int_lit(3), // Periodic extrapolation
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);
    env.set("table_id", table_id);

    let next_after_end = eval_expr::<f64>(
        &fn_call("getNextTimeEvent", vec![var("table_id"), lit(2.25)]),
        &env,
    );
    assert!((next_after_end - 4.0).abs() < 1e-12);
}

#[test]
fn test_string_is_empty_runtime_special() {
    let env = VarEnv::<f64>::new();
    let empty = eval_expr::<f64>(
        &fn_call(
            "Modelica.Utilities.Strings.isEmpty",
            vec![dae::Expression::Literal(dae::Literal::String(
                "   ".to_string(),
            ))],
        ),
        &env,
    );
    assert_eq!(empty, 1.0);

    let non_empty = eval_expr::<f64>(
        &fn_call(
            "Modelica.Utilities.Strings.isEmpty",
            vec![dae::Expression::Literal(dae::Literal::String(
                "abc".to_string(),
            ))],
        ),
        &env,
    );
    assert_eq!(non_empty, 0.0);
}

#[test]
fn test_random_runtime_special_seed_and_stream() {
    let env = VarEnv::<f64>::new();
    let seed = eval_expr::<f64>(&fn_call("automaticGlobalSeed", vec![]), &env);
    assert!(seed.is_finite());
    assert!(seed >= 1.0);

    let id = eval_expr::<f64>(&fn_call("initializeImpureRandom", vec![lit(seed)]), &env);
    assert!(id.is_finite());
    assert!(id >= 1.0);

    let r1 = eval_expr::<f64>(&fn_call("impureRandom", vec![lit(id)]), &env);
    let r2 = eval_expr::<f64>(&fn_call("impureRandom", vec![lit(id)]), &env);
    assert!(r1 > 0.0 && r1 <= 1.0);
    assert!(r2 > 0.0 && r2 <= 1.0);
    assert!(
        (r1 - r2).abs() > 1e-15,
        "impureRandom should advance stream state"
    );
}

#[test]
fn test_random_runtime_special_automatic_local_seed_is_stable_for_literal_path() {
    let env = VarEnv::<f64>::new();
    let a = eval_expr::<f64>(
        &fn_call(
            "automaticLocalSeed",
            vec![dae::Expression::Literal(dae::Literal::String(
                "Modelica.Blocks.Examples.Noise.UniformNoise".to_string(),
            ))],
        ),
        &env,
    );
    let b = eval_expr::<f64>(
        &fn_call(
            "automaticLocalSeed",
            vec![dae::Expression::Literal(dae::Literal::String(
                "Modelica.Blocks.Examples.Noise.UniformNoise".to_string(),
            ))],
        ),
        &env,
    );
    assert_eq!(a, b);
    assert!(a >= 1.0);
}

#[test]
fn test_random_runtime_special_qualified_xorshift_calls_are_supported() {
    let mut env = VarEnv::<f64>::new();
    env.set("state", 12345.0);

    let init = eval_expr::<f64>(
        &fn_call(
            "Modelica.Math.Random.Generators.Xorshift128plus.initialState",
            vec![int_lit(10), int_lit(20)],
        ),
        &env,
    );
    assert!(init.is_finite() && init >= 1.0);

    let r = eval_expr::<f64>(
        &fn_call(
            "Modelica.Math.Random.Generators.Xorshift64star.random",
            vec![var("state")],
        ),
        &env,
    );
    assert!(r > 0.0 && r <= 1.0);
}

#[test]
fn test_table1d_default_columns_skip_abscissa() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            dae::Expression::Empty, // no explicit columns mapping
            int_lit(1),             // LinearSegments
            int_lit(1),             // HoldLastPoint
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);
    env.set("table_id", table_id);
    env.set("u", 1.0);

    let y = eval_expr::<f64>(
        &fn_call(
            "getTable1DValueNoDer",
            vec![var("table_id"), int_lit(1), var("u")],
        ),
        &env,
    );
    assert!((y - 12.0).abs() < 1e-12);
}
