use super::*;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("eval_flat_phase_constant_source_7.mo"),
        0,
        1,
    )
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn indexed_var(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![rumoca_core::Subscript::index(
            index,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    }
}

fn index_expr(base: rumoca_core::Expression, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(base),
        subscripts: vec![rumoca_core::Subscript::index(
            index,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    }
}

fn comp_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: Vec::new(),
        }],
        def_id: None,
    }
}

fn field(base: rumoca_core::Expression, name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: name.to_string(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn int(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn real(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn binary(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn empty_param_context<'a>(
    known_ints: &'a FxHashMap<String, i64>,
    known_reals: &'a FxHashMap<String, f64>,
    known_bools: &'a FxHashMap<String, bool>,
    known_enums: &'a FxHashMap<String, String>,
    array_dims: &'a FxHashMap<String, Vec<i64>>,
    functions: &'a FxHashMap<String, rumoca_core::Function>,
) -> ParamEvalContext<'a> {
    ParamEvalContext {
        known_ints,
        known_reals,
        known_bools,
        known_enums,
        array_dims,
        functions,
        user_func_eval_ctx: None,
        var_context: None,
    }
}

fn call(
    function: rumoca_core::BuiltinFunction,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function,
        args,
        span: rumoca_core::Span::DUMMY,
    }
}

fn function_call(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(name),
        args,
        is_constructor: false,
        span: test_span(),
    }
}

fn scalar_indexing_function(
    name: &str,
    input_type: &str,
    output_type: &str,
) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new(name, test_span());
    function.add_input(rumoca_core::FunctionParam::new(
        "x",
        input_type,
        test_span(),
    ));
    function.add_output(rumoca_core::FunctionParam::new(
        "y",
        output_type,
        test_span(),
    ));
    function.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("y"),
        value: indexed_var("x", 1),
        span: rumoca_core::Span::DUMMY,
    }];
    function
}

#[test]
fn eval_integer_div_operator_requires_exact_quotient() {
    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Div,
        lhs: Box::new(int(7)),
        rhs: Box::new(int(2)),
        span: rumoca_core::Span::DUMMY,
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
        user_func_eval_ctx: None,
        var_context: None,
    };
    assert_eq!(try_eval_integer_with_context(&expr, &ctx), None);
}

#[test]
fn eval_integer_exponentiation_for_structural_dimensions() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("n_addr".to_string(), 2);
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    let expr = call(
        rumoca_core::BuiltinFunction::Integer,
        vec![binary(rumoca_core::OpBinary::Exp, int(2), var("n_addr"))],
    );

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(4));
}

#[test]
fn infer_user_function_output_dims_from_shape_expr() {
    let mut functions = FxHashMap::default();
    let mut function = rumoca_core::Function::new("Pkg.indices", test_span());
    function.add_input(rumoca_core::FunctionParam::new("m", "Integer", test_span()));
    function.add_output(
        rumoca_core::FunctionParam::new("ind", "Integer", test_span())
            .with_dims(vec![0])
            .with_shape_expr(vec![rumoca_core::Subscript::expr(
                Box::new(binary(rumoca_core::OpBinary::Add, var("m"), int(1))),
                rumoca_core::Span::DUMMY,
            )]),
    );
    functions.insert("Pkg.indices".to_string(), function);

    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let expr = function_call("Pkg.indices", vec![int(4)]);

    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &expr,
            &ParamEvalContext::new(
                &known_ints,
                &known_reals,
                &known_bools,
                &known_enums,
                &array_dims,
                &functions,
                None,
            ),
        ),
        Some(vec![5])
    );
}

#[test]
fn infer_scalar_user_function_broadcasts_array_argument_dims() {
    let mut functions = FxHashMap::default();
    let mut function = rumoca_core::Function::new("Cv.from_deg", test_span());
    function.add_input(rumoca_core::FunctionParam::new(
        "degree",
        "Real",
        test_span(),
    ));
    function.add_output(rumoca_core::FunctionParam::new(
        "radian",
        "Real",
        test_span(),
    ));
    functions.insert("Cv.from_deg".to_string(), function);

    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let mut array_dims = FxHashMap::default();
    array_dims.insert("pathPlanning.angleBegDeg".to_string(), vec![6]);
    let expr = function_call("Cv.from_deg", vec![var("angleBegDeg")]);

    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &expr,
            &ParamEvalContext::new(
                &known_ints,
                &known_reals,
                &known_bools,
                &known_enums,
                &array_dims,
                &functions,
                Some("pathPlanning.angleBeg"),
            ),
        ),
        Some(vec![6])
    );
}

#[test]
fn user_function_integer_eval_error_means_not_constant_evaluable() {
    let mut functions = FxHashMap::default();
    functions.insert(
        "Pkg.badInteger".to_string(),
        scalar_indexing_function("Pkg.badInteger", "Integer", "Integer"),
    );
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    let expr = function_call("Pkg.badInteger", vec![int(3)]);

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), None);
}

#[test]
fn user_function_real_eval_error_means_not_constant_evaluable() {
    let mut functions = FxHashMap::default();
    functions.insert(
        "Pkg.badReal".to_string(),
        scalar_indexing_function("Pkg.badReal", "Real", "Real"),
    );
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    assert_eq!(
        eval_user_func_real(
            &rumoca_core::Reference::new("Pkg.badReal"),
            &[real(3.0)],
            &ctx,
        ),
        None
    );
}

#[test]
fn suffix_integer_lookup_rejects_ambiguous_matches() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("outer.n".to_string(), 2);
    known_ints.insert("n".to_string(), 3);
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    assert_eq!(
        try_eval_integer_with_context(&var("model.outer.n"), &ctx),
        None
    );
}

#[test]
fn suffix_integer_lookup_allows_unique_overqualified_match() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("outer.n".to_string(), 2);
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    assert_eq!(
        try_eval_integer_with_context(&var("model.outer.n"), &ctx),
        Some(2)
    );
}

#[test]
fn eval_integer_div_builtin_remains_truncating() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Div,
        args: vec![int(7), int(2)],
        span: rumoca_core::Span::DUMMY,
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
        user_func_eval_ctx: None,
        var_context: None,
    };
    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(3));
}

#[test]
fn eval_integer_mod_builtin_uses_floor_semantics() {
    let expr = call(rumoca_core::BuiltinFunction::Mod, vec![int(-7), int(3)]);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(2));
}

#[test]
fn eval_integer_rem_builtin_remains_truncating() {
    let expr = call(rumoca_core::BuiltinFunction::Rem, vec![int(-7), int(3)]);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(-1));
}

#[test]
fn eval_max_min_integer_uses_full_structural_context() {
    let mut known_reals = FxHashMap::default();
    known_reals.insert("f_max".to_string(), 4.0);
    known_reals.insert("f_resolution".to_string(), 0.2);
    let mut known_ints = FxHashMap::default();
    known_ints.insert("nf".to_string(), 50);
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    let frequency_ratio = binary(
        rumoca_core::OpBinary::Div,
        var("f_max"),
        var("f_resolution"),
    );
    let ceil_ratio = call(rumoca_core::BuiltinFunction::Ceil, vec![frequency_ratio]);
    let integer_ceil_ratio = call(rumoca_core::BuiltinFunction::Integer, vec![ceil_ratio]);
    let selected = call(
        rumoca_core::BuiltinFunction::Min,
        vec![
            binary(rumoca_core::OpBinary::Add, integer_ceil_ratio, int(1)),
            var("nf"),
        ],
    );
    let expr = call(rumoca_core::BuiltinFunction::Max, vec![int(1), selected]);

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(21));
}

#[test]
fn eval_boolean_enum_eq_accepts_different_qualification_paths() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "pipe.modelStructure".to_string(),
        "Modelica.Fluid.Types.ModelStructure.a_vb".to_string(),
    );

    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("pipe.modelStructure")),
        rhs: Box::new(var("pipe.Types.ModelStructure.a_vb")),
        span: rumoca_core::Span::DUMMY,
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

    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("frameResolve")),
        rhs: Box::new(var(
            "Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve",
        )),
        span: rumoca_core::Span::DUMMY,
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

    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("mode")),
        rhs: Box::new(var("Modelica.Blocks.Types.SimpleController.PI")),
        span: rumoca_core::Span::DUMMY,
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

    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("pipe.modelStructure")),
        rhs: Box::new(var("pipe.Types.ModelStructure.a_vb")),
        span: rumoca_core::Span::DUMMY,
    };

    let expr = rumoca_core::Expression::If {
        branches: vec![(
            cond,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(var("pipe.n")),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
        )],
        else_branch: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &known_enums,
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        user_func_eval_ctx: None,
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

    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Or,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandPass")),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandStop")),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };

    let expr = rumoca_core::Expression::If {
        branches: vec![(
            cond,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(int(2)),
                rhs: Box::new(var("order")),
                span: rumoca_core::Span::DUMMY,
            },
        )],
        else_branch: Box::new(var("order")),
        span: rumoca_core::Span::DUMMY,
    };

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &known_enums,
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        user_func_eval_ctx: None,
        var_context: Some("Bessel.na"),
    };

    let value = try_eval_integer_with_context(&expr, &ctx);
    assert_eq!(value, Some(3));
}

#[test]
fn eval_integer_prefers_scoped_unqualified_name_over_global_name() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("m".to_string(), 3);
    known_ints.insert("machine.rotor.converter.m".to_string(), 2);

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        user_func_eval_ctx: None,
        var_context: Some("machine.rotor.converter.orientation"),
    };

    assert_eq!(try_eval_integer_with_context(&var("m"), &ctx), Some(2));
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

    let filter_is_band = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Or,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandPass")),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandStop")),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };

    let analog_is_cd = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("analogFilter")),
        rhs: Box::new(var("Modelica.Blocks.Types.AnalogFilter.CriticalDamping")),
        span: rumoca_core::Span::DUMMY,
    };

    let expr = rumoca_core::Expression::If {
        branches: vec![(filter_is_band, var("order")), (analog_is_cd, int(0))],
        else_branch: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Integer,
            args: vec![rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Div,
                lhs: Box::new(var("order")),
                rhs: Box::new(int(2)),
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &known_enums,
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        user_func_eval_ctx: None,
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

    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("pipe.energyDynamics")),
        rhs: Box::new(var("pipe1.system.energyDynamics")),
        span: rumoca_core::Span::DUMMY,
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
        user_func_eval_ctx: None,
        var_context: Some("stack.cell[1,1].cell.cellData.nRC"),
    };

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(2));
}

#[test]
fn eval_integer_nout_resolves_from_scoped_columns_dimension() {
    let known_ints = FxHashMap::default();
    let mut array_dims = FxHashMap::default();
    array_dims.insert("stack.cell[1,2].cell.ocv_soc.columns".to_string(), vec![1]);

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &FxHashMap::default(),
        array_dims: &array_dims,
        functions: &FxHashMap::default(),
        user_func_eval_ctx: None,
        var_context: Some("stack.cell[1,2].cell.ocv_soc.y"),
    };

    assert_eq!(try_eval_integer_with_context(&var("nout"), &ctx), Some(1));
}

#[test]
fn eval_integer_if_returns_common_value_when_condition_unknown() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("left".to_string(), 2);
    known_ints.insert("right".to_string(), 2);

    let expr = rumoca_core::Expression::If {
        branches: vec![(var("cond"), var("left"))],
        else_branch: Box::new(var("right")),
        span: rumoca_core::Span::DUMMY,
    };
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        user_func_eval_ctx: None,
        var_context: None,
    };

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(2));
}

#[test]
fn component_path_parent_ignores_dot_inside_subscript_expression() {
    assert_eq!(
        rumoca_core::ComponentPath::from_flat_path("arr[data.medium]")
            .parent()
            .map(|path| path.to_flat_string()),
        Some(String::new())
    );
    assert_eq!(
        rumoca_core::ComponentPath::from_flat_path("pkg.arr[data.medium]")
            .parent()
            .map(|path| path.to_flat_string())
            .as_deref(),
        Some("pkg")
    );
    assert_eq!(
        rumoca_core::ComponentPath::from_flat_path("pkg.arr[data.medium].field")
            .parent()
            .map(|path| path.to_flat_string())
            .as_deref(),
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

    let expr = rumoca_core::Expression::If {
        branches: vec![(var("Medium.singleState"), var("Dynamics.SteadyState"))],
        else_branch: Box::new(var("Dynamics.SteadyStateInitial")),
        span: rumoca_core::Span::DUMMY,
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
    let expr = rumoca_core::Expression::If {
        branches: vec![(var("cond"), var("Dynamics.SteadyState"))],
        else_branch: Box::new(var("Dynamics.SteadyState")),
        span: rumoca_core::Span::DUMMY,
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

    let expr = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("n")),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
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
fn infer_array_dims_with_context_resolves_scoped_if_matrix_columns() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("booleanTable.n".to_string(), 7);
    let mut array_dims = FxHashMap::default();
    array_dims.insert("booleanTable.table".to_string(), vec![7]);

    let table_column = var("table");
    let generated_column = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("n")),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };
    let then_matrix = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![indexed_var("booleanTable.table", 1), int(0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![table_column, generated_column],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    };
    let else_matrix = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::Array {
            elements: vec![int(0), int(0)],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::If {
        branches: vec![(
            binary(rumoca_core::OpBinary::Gt, var("n"), int(0)),
            then_matrix,
        )],
        else_branch: Box::new(else_matrix),
        span: rumoca_core::Span::DUMMY,
    };

    let dims = infer_array_dimensions_full_with_functions(
        &expr,
        &ParamEvalContext::new(
            &known_ints,
            &FxHashMap::default(),
            &FxHashMap::default(),
            &FxHashMap::default(),
            &array_dims,
            &FxHashMap::default(),
            Some("booleanTable.combiTimeTable.table"),
        ),
    );

    assert_eq!(dims, Some(vec![8, 2]));
}

#[test]
fn infer_array_dims_with_context_preserves_vector_column_rows() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("booleanTable.n".to_string(), 7);
    let mut known_bools = FxHashMap::default();
    known_bools.insert("booleanTable.startValue".to_string(), false);
    let mut array_dims = FxHashMap::default();
    array_dims.insert("booleanTable.table".to_string(), vec![7]);

    let generated_column = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("n")),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };
    let vector_column_matrix = rumoca_core::Expression::Array {
        elements: vec![var("table"), generated_column],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::If {
        branches: vec![(var("startValue"), int(0))],
        else_branch: Box::new(vector_column_matrix),
        span: rumoca_core::Span::DUMMY,
    };

    let dims = infer_array_dimensions_full_with_functions(
        &expr,
        &ParamEvalContext::new(
            &known_ints,
            &FxHashMap::default(),
            &known_bools,
            &FxHashMap::default(),
            &array_dims,
            &FxHashMap::default(),
            Some("booleanTable.combiTimeTable.table"),
        ),
    );

    assert_eq!(dims, Some(vec![7, 2]));
}

#[test]
fn infer_array_dims_with_context_resolves_boolean_table_binding() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("booleanTable.n".to_string(), 7);
    let mut known_bools = FxHashMap::default();
    known_bools.insert("booleanTable.startValue".to_string(), false);
    let mut array_dims = FxHashMap::default();
    array_dims.insert("booleanTable.table".to_string(), vec![7]);

    let generated_column = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(call(
            rumoca_core::BuiltinFunction::Mod,
            vec![var("i"), real(2.0)],
        )),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("booleanTable.n")),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };
    let false_branch = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![index_expr(var("booleanTable.table"), 1), real(0.0)],
                is_matrix: true,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![var("booleanTable.table"), generated_column],
                is_matrix: true,
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = rumoca_core::Expression::If {
        branches: vec![(
            binary(rumoca_core::OpBinary::Gt, var("booleanTable.n"), int(0)),
            rumoca_core::Expression::If {
                branches: vec![(var("booleanTable.startValue"), int(0))],
                else_branch: Box::new(false_branch),
                span: rumoca_core::Span::DUMMY,
            },
        )],
        else_branch: Box::new(int(0)),
        span: rumoca_core::Span::DUMMY,
    };

    let dims = infer_array_dimensions_full_with_functions(
        &expr,
        &ParamEvalContext::new(
            &known_ints,
            &FxHashMap::default(),
            &known_bools,
            &FxHashMap::default(),
            &array_dims,
            &FxHashMap::default(),
            Some("booleanTable.combiTimeTable.table"),
        ),
    );

    assert_eq!(dims, Some(vec![8, 2]));
}

#[test]
fn infer_array_dims_from_nested_comprehension_body_shape() {
    let expr = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(rumoca_core::Expression::Array {
            elements: vec![var("i"), var("i")],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(int(3)),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    let dims = infer_array_dimensions(&expr);
    assert_eq!(dims, Some(vec![3, 2]));
}

#[test]
fn infer_array_dims_vector_of_vertical_matrix_concat_uses_operand_shapes() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("na".to_string(), 3);
    known_ints.insert("nb".to_string(), 2);
    let mut array_dims = FxHashMap::default();
    array_dims.insert("b".to_string(), vec![2]);

    let zero_rows = call(
        rumoca_core::BuiltinFunction::Zeros,
        vec![
            call(
                rumoca_core::BuiltinFunction::Max,
                vec![
                    int(0),
                    rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Sub,
                        lhs: Box::new(var("na")),
                        rhs: Box::new(var("nb")),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
            ),
            int(1),
        ],
    );
    let matrix = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![zero_rows],
                is_matrix: true,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Array {
                elements: vec![var("b")],
                is_matrix: true,
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: true,
        span: rumoca_core::Span::DUMMY,
    };
    let expr = call(rumoca_core::BuiltinFunction::Vector, vec![matrix]);

    assert_eq!(
        infer_array_dimensions_full_with_conds(
            &expr,
            &known_ints,
            &FxHashMap::default(),
            &FxHashMap::default(),
            &array_dims,
        ),
        Some(vec![3]),
        "MLS §10.4.2 matrix constructors concatenate array operands before vector() flattens them"
    );
}

#[test]
fn infer_array_dims_from_comprehension_returns_none_with_filter() {
    let expr = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(int(3)),
                span: rumoca_core::Span::DUMMY,
            },
        }],
        filter: Some(Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(var("i")),
            rhs: Box::new(int(1)),
            span: rumoca_core::Span::DUMMY,
        })),
        span: rumoca_core::Span::DUMMY,
    };

    let dims = infer_array_dimensions(&expr);
    assert_eq!(dims, None);
}
