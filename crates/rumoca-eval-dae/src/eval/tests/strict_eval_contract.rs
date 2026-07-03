use super::*;

struct DefaultZeroSurface {
    file: &'static str,
    source: &'static str,
    expected_matches: usize,
}

#[test]
fn default_zero_eval_surface_does_not_grow() {
    let surfaces = [
        DefaultZeroSurface {
            file: "eval/builtin_table.rs",
            source: include_str!("../builtin_table.rs"),
            expected_matches: 3,
        },
        DefaultZeroSurface {
            file: "eval/clock_eval.rs",
            source: include_str!("../clock_eval.rs"),
            expected_matches: 3,
        },
        DefaultZeroSurface {
            file: "eval/distribution_clock.rs",
            source: include_str!("../distribution_clock.rs"),
            expected_matches: 13,
        },
        DefaultZeroSurface {
            file: "eval/eval_expr_impl.rs",
            source: include_str!("../eval_expr_impl.rs"),
            expected_matches: 14,
        },
        DefaultZeroSurface {
            file: "eval/builtin_runtime.rs",
            source: include_str!("../builtin_runtime.rs"),
            expected_matches: 7,
        },
        DefaultZeroSurface {
            file: "eval/mod.rs",
            source: include_str!("../mod.rs"),
            expected_matches: 3,
        },
        DefaultZeroSurface {
            file: "eval/special.rs",
            source: include_str!("../special.rs"),
            expected_matches: 6,
        },
        DefaultZeroSurface {
            file: "eval/table_eval.rs",
            source: include_str!("../table_eval.rs"),
            expected_matches: 3,
        },
    ];

    for surface in surfaces {
        let actual = default_zero_match_count(surface.source);
        assert!(
            actual <= surface.expected_matches,
            "{} grew from {} to {actual} default-zero fallback patterns",
            surface.file,
            surface.expected_matches
        );
    }
}

#[test]
fn production_statement_eval_does_not_call_default_evaluator() {
    let source = include_str!("../../statement.rs");
    let production_source = source
        .split("#[cfg(test)]")
        .next()
        .expect("statement source should include production section");

    assert!(
        !production_source.contains("eval_expr_value"),
        "production algorithm statement evaluation must use fallible expression evaluation"
    );
}

fn default_zero_match_count(source: &str) -> usize {
    let patterns = [
        "T::zero()",
        "unwrap_or_else(T::zero)",
        "unwrap_or(T::zero())",
        "map_or_else(T::zero,",
    ];

    source
        .lines()
        .filter(|line| patterns.iter().any(|pattern| line.contains(pattern)))
        .count()
}

#[test]
fn eval_const_expr_rejects_missing_binding_instead_of_defaulting_zero() {
    assert_eq!(
        eval_const_expr(&var("missing")),
        Err(EvalError::MissingBinding {
            name: "missing".to_string(),
        })
    );
}

#[test]
fn eval_const_expr_rejects_unsupported_scalar_form() {
    let range = rumoca_core::Expression::Range {
        start: Box::new(int_lit(1)),
        step: None,
        end: Box::new(int_lit(3)),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_const_expr(&range),
        Err(EvalError::UnsupportedExpression { kind: "range" })
    );
}

#[test]
fn eval_expr_rejects_string_literal_in_scalar_path() {
    let expr = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String("abc".to_string()),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&expr, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "string literal",
        })
    );
}

#[test]
fn eval_expr_rejects_placeholder_binary_operators() {
    let expr = binop(OpBinary::Assign, lit(1.0), lit(2.0));

    assert_eq!(
        eval_expr::<f64>(&expr, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "placeholder binary operator",
        })
    );
}

#[test]
fn eval_expr_rejects_missing_index_expression_in_var_ref() {
    let indexed = rumoca_core::Expression::VarRef {
        name: Reference::new("x"),
        subscripts: vec![Subscript::generated_expr(
            Box::new(var("missing")),
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&indexed, &VarEnv::new()),
        Err(EvalError::MissingBinding {
            name: "missing".to_string(),
        })
    );
}

#[test]
fn eval_expr_rejects_colon_subscript_in_scalar_var_ref() {
    let indexed = rumoca_core::Expression::VarRef {
        name: Reference::new("x"),
        subscripts: vec![Subscript::generated_colon(rumoca_core::Span::DUMMY)],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&indexed, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "colon subscript",
        })
    );
}

#[test]
fn try_eval_array_like_rejects_cat_dim2_row_mismatch() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Cat,
        args: vec![
            int_lit(2),
            arr(
                vec![
                    arr(vec![lit(1.0), lit(2.0)], false),
                    arr(vec![lit(3.0), lit(4.0)], false),
                ],
                true,
            ),
            arr(vec![arr(vec![lit(5.0)], false)], true),
        ],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_array_like_values::<f64>(&expr, &VarEnv::new()),
        Err(EvalError::ShapeMismatch {
            context: "cat rows",
            expected: 2,
            actual: 1,
        })
    );
}

#[test]
fn try_eval_array_like_rejects_invalid_cat_dimension() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Cat,
        args: vec![
            lit(1.5),
            arr(vec![lit(1.0), lit(2.0)], false),
            arr(vec![lit(3.0), lit(4.0)], false),
        ],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_array_like_values::<f64>(&expr, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "cat dimension",
        })
    );
}

#[test]
fn eval_array_like_rejects_missing_array_constructor_dimensions() {
    assert_eq!(
        eval_array_like_values::<f64>(&builtin(BuiltinFunction::Zeros, vec![]), &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "zeros arguments",
        })
    );
    assert_eq!(
        eval_array_like_values::<f64>(&builtin(BuiltinFunction::Ones, vec![]), &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "ones arguments",
        })
    );
    assert_eq!(
        eval_array_like_values::<f64>(
            &builtin(BuiltinFunction::Fill, vec![lit(1.0)]),
            &VarEnv::new()
        ),
        Err(EvalError::UnsupportedExpression {
            kind: "fill arguments",
        })
    );
}

#[test]
fn eval_array_like_rejects_invalid_array_constructor_dimensions() {
    assert_eq!(
        eval_array_like_values::<f64>(
            &builtin(BuiltinFunction::Zeros, vec![lit(1.5)]),
            &VarEnv::new(),
        ),
        Err(EvalError::UnsupportedExpression {
            kind: "array constructor dimension",
        })
    );
    assert_eq!(
        eval_array_like_values::<f64>(
            &builtin(BuiltinFunction::Ones, vec![lit(-1.0)]),
            &VarEnv::new(),
        ),
        Err(EvalError::UnsupportedExpression {
            kind: "array constructor dimension",
        })
    );
}

#[test]
fn eval_expr_accepts_checked_index_on_matrix_literal() {
    let indexed = rumoca_core::Expression::Index {
        base: Box::new(simple_table_expr()),
        subscripts: vec![
            Subscript::generated_index(2, rumoca_core::Span::DUMMY),
            Subscript::generated_index(1, rumoca_core::Span::DUMMY),
        ],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&indexed, &VarEnv::new()), Ok(2.0));
}

#[test]
fn eval_expr_rejects_missing_index_expression_in_index() {
    let indexed = rumoca_core::Expression::Index {
        base: Box::new(var("x")),
        subscripts: vec![Subscript::generated_expr(
            Box::new(var("missing")),
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&indexed, &VarEnv::new()),
        Err(EvalError::MissingBinding {
            name: "missing".to_string(),
        })
    );
}

#[test]
fn eval_expr_rejects_colon_index_in_scalar_index() {
    let indexed = rumoca_core::Expression::Index {
        base: Box::new(var("x")),
        subscripts: vec![Subscript::generated_colon(rumoca_core::Span::DUMMY)],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&indexed, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "colon index",
        })
    );
}

#[test]
fn eval_expr_rejects_sparse_declared_indexed_env_binding() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("A".to_string(), vec![2])]));
    env.set("A[1]", 10.0);

    let indexed = rumoca_core::Expression::Index {
        base: Box::new(var("A")),
        subscripts: vec![Subscript::generated_index(2, rumoca_core::Span::DUMMY)],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&indexed, &env),
        Err(EvalError::ShapeMismatch {
            context: "declared array dimensions",
            expected: 2,
            actual: 1,
        })
    );
}

#[test]
fn eval_expr_rejects_missing_indexed_env_binding() {
    let mut env = VarEnv::<f64>::new();
    env.set("A[1]", 10.0);

    let indexed = rumoca_core::Expression::Index {
        base: Box::new(var("A")),
        subscripts: vec![Subscript::generated_index(2, rumoca_core::Span::DUMMY)],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&indexed, &env),
        Err(EvalError::MissingBinding {
            name: "A[2]".to_string(),
        })
    );
}

#[test]
fn eval_expr_rejects_out_of_range_indexed_env_binding() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("A".to_string(), vec![2])]));
    env.set("A[1]", 10.0);
    env.set("A[2]", 20.0);

    let indexed = rumoca_core::Expression::Index {
        base: Box::new(var("A")),
        subscripts: vec![Subscript::generated_index(3, rumoca_core::Span::DUMMY)],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&indexed, &env),
        Err(EvalError::MissingBinding {
            name: "A[3]".to_string(),
        })
    );
}

#[test]
fn eval_expr_accepts_indexed_record_field_without_aggregate_slot() {
    let mut env = VarEnv::<f64>::new();
    env.set("records[1].flag", 7.0);

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(var("records")),
            subscripts: vec![Subscript::generated_index(1, rumoca_core::Span::DUMMY)],
            span: rumoca_core::Span::DUMMY,
        }),
        field: "flag".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), Ok(7.0));
}

#[test]
fn eval_expr_rejects_missing_indexed_record_field_binding() {
    let env = VarEnv::<f64>::new();

    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::Index {
            base: Box::new(var("records")),
            subscripts: vec![Subscript::generated_index(1, rumoca_core::Span::DUMMY)],
            span: rumoca_core::Span::DUMMY,
        }),
        field: "flag".to_string(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&expr, &env),
        Err(EvalError::MissingBinding {
            name: "records[1].flag".to_string(),
        })
    );
}

#[test]
fn eval_expr_rejects_missing_external_table_matrix_binding() {
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            var("missing_table"),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );

    assert_eq!(
        eval_expr::<f64>(&constructor, &VarEnv::new()),
        Err(EvalError::MissingBinding {
            name: "missing_table".to_string(),
        })
    );
}

#[test]
fn eval_expr_rejects_missing_external_table_constructor_matrix_arg() {
    let constructor = fn_call("ExternalCombiTable1D", vec![lit(0.0), lit(0.0)]);

    assert_eq!(
        eval_expr::<f64>(&constructor, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "external table constructor",
        })
    );
}

#[test]
fn eval_expr_accepts_empty_external_table_constructor_data() {
    let empty_table = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![lit(0.0), int_lit(0), int_lit(2)],
        span: rumoca_core::Span::DUMMY,
    };
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("NoName".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("NoName".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            empty_table,
            lit(0.0),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );

    let table_id = eval_expr::<f64>(&constructor, &VarEnv::new())
        .expect("empty external table constructors should still register a table id");

    assert!(table_id > 0.0);
}

#[test]
fn eval_expr_rejects_one_column_external_table_data() {
    let one_column_table = arr(
        vec![arr(vec![lit(0.0)], false), arr(vec![lit(1.0)], false)],
        true,
    );
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            one_column_table,
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );

    assert_eq!(
        eval_expr::<f64>(&constructor, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "external table data",
        })
    );
}

#[test]
fn eval_expr_rejects_ragged_external_table_data() {
    let ragged_table = arr(
        vec![
            arr(vec![lit(0.0), lit(10.0)], false),
            arr(vec![lit(1.0)], false),
        ],
        true,
    );
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            ragged_table,
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );

    assert_eq!(
        eval_expr::<f64>(&constructor, &VarEnv::new()),
        Err(EvalError::ShapeMismatch {
            context: "matrix literal",
            expected: 2,
            actual: 1,
        })
    );
}

#[test]
fn eval_expr_rejects_out_of_range_external_table_column() {
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            arr(vec![int_lit(3)], false),
            int_lit(1),
            int_lit(1),
        ],
    );

    assert_eq!(
        eval_expr::<f64>(&constructor, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "external table columns",
        })
    );
}

#[test]
fn eval_expr_rejects_missing_required_user_function_input() {
    let mut env = VarEnv::<f64>::new();
    let mut function = Function::new("Pkg.required", rumoca_core::Span::DUMMY);
    function.add_input(FunctionParam::new(
        "u",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.add_output(
        FunctionParam::new("y", "Real", rumoca_core::Span::source_free_serde_default())
            .with_default(var("u")),
    );
    function.body = vec![Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    env.functions = Arc::new(IndexMap::from([("Pkg.required".to_string(), function)]));

    let call = fn_call("Pkg.required", vec![]);

    assert_eq!(
        eval_expr::<f64>(&call, &env),
        Err(EvalError::MissingBinding {
            name: "Pkg.required.u".to_string(),
        })
    );
}

#[test]
fn eval_expr_accepts_supplied_required_user_function_input() {
    let mut env = VarEnv::<f64>::new();
    let mut function = Function::new("Pkg.required", rumoca_core::Span::DUMMY);
    function.add_input(FunctionParam::new(
        "u",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.add_output(
        FunctionParam::new("y", "Real", rumoca_core::Span::source_free_serde_default())
            .with_default(var("u")),
    );
    function.body = vec![Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    env.functions = Arc::new(IndexMap::from([("Pkg.required".to_string(), function)]));

    let call = fn_call("Pkg.required", vec![lit(4.5)]);

    assert_eq!(eval_expr::<f64>(&call, &env), Ok(4.5));
}

#[test]
fn eval_expr_rejects_unknown_named_user_function_input() {
    let mut env = VarEnv::<f64>::new();
    let mut function = Function::new("Pkg.required", rumoca_core::Span::DUMMY);
    function.add_input(FunctionParam::new(
        "u",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.add_output(
        FunctionParam::new("y", "Real", rumoca_core::Span::source_free_serde_default())
            .with_default(var("u")),
    );
    function.body = vec![Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    env.functions = Arc::new(IndexMap::from([("Pkg.required".to_string(), function)]));

    let call = fn_call("Pkg.required", vec![named_ctor_arg("v", lit(4.5))]);

    assert_eq!(
        eval_expr::<f64>(&call, &env),
        Err(EvalError::UnsupportedExpression {
            kind: "function call named argument",
        })
    );
}

#[test]
fn eval_expr_rejects_missing_binding_in_user_function_default_input() {
    let mut env = VarEnv::<f64>::new();
    let mut function = Function::new("Pkg.defaults", rumoca_core::Span::DUMMY);
    function.add_input(FunctionParam::new(
        "a",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.add_input(
        FunctionParam::new("b", "Real", rumoca_core::Span::source_free_serde_default())
            .with_default(var("missing")),
    );
    function.add_output(
        FunctionParam::new("y", "Real", rumoca_core::Span::source_free_serde_default())
            .with_default(var("b")),
    );
    function.body = vec![Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    env.functions = Arc::new(IndexMap::from([("Pkg.defaults".to_string(), function)]));

    let call = fn_call("Pkg.defaults", vec![lit(2.0)]);

    assert_eq!(
        eval_expr::<f64>(&call, &env),
        Err(EvalError::MissingBinding {
            name: "missing".to_string(),
        })
    );
}

#[test]
fn eval_expr_accepts_user_function_default_input_referencing_prior_input() {
    let mut env = VarEnv::<f64>::new();
    let mut function = Function::new("Pkg.defaults", rumoca_core::Span::DUMMY);
    function.add_input(FunctionParam::new(
        "a",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.add_input(
        FunctionParam::new("b", "Real", rumoca_core::Span::source_free_serde_default())
            .with_default(binop(OpBinary::Add, var("a"), lit(1.5))),
    );
    function.add_output(
        FunctionParam::new("y", "Real", rumoca_core::Span::source_free_serde_default())
            .with_default(var("b")),
    );
    function.body = vec![Statement::Empty {
        span: rumoca_core::Span::DUMMY,
    }];
    env.functions = Arc::new(IndexMap::from([("Pkg.defaults".to_string(), function)]));

    let call = fn_call("Pkg.defaults", vec![lit(2.0)]);

    assert_eq!(eval_expr::<f64>(&call, &env), Ok(3.5));
}

#[test]
fn eval_expr_rejects_zero_step_range_in_checked_array_builtin() {
    let range = rumoca_core::Expression::Range {
        start: Box::new(int_lit(1)),
        step: Some(Box::new(int_lit(0))),
        end: Box::new(int_lit(3)),
        span: rumoca_core::Span::DUMMY,
    };
    let sum = rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Sum,
        args: vec![range],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&sum, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression { kind: "range" })
    );
}

#[test]
fn eval_expr_rejects_malformed_builtin_arity_with_span() {
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("arity.mo"), 4, 12);
    let expr = rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Atan2,
        args: vec![lit(1.0)],
        span,
    };

    let err = eval_expr::<f64>(&expr, &VarEnv::new()).expect_err("atan2 arity must fail");
    assert_eq!(
        err,
        EvalError::UnsupportedExpression {
            kind: "builtin arity",
        }
        .with_span_if_missing(span)
    );
    assert_eq!(err.source_span(), Some(span));
}

#[test]
fn eval_expr_rejects_malformed_homotopy_arity() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Homotopy,
        args: vec![],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&expr, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "homotopy arity",
        })
    );
}

#[test]
fn eval_expr_accepts_valid_range_in_checked_array_builtin() {
    let range = rumoca_core::Expression::Range {
        start: Box::new(int_lit(1)),
        step: Some(Box::new(int_lit(1))),
        end: Box::new(int_lit(3)),
        span: rumoca_core::Span::DUMMY,
    };
    let sum = rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Sum,
        args: vec![range],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&sum, &VarEnv::new()), Ok(6.0));
}

#[test]
fn eval_expr_rejects_empty_interleaved_matrix_column() {
    let matrix = arr(vec![lit(1.0), arr(Vec::new(), false)], true);
    let sum = rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Sum,
        args: vec![matrix],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&sum, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "matrix literal",
        })
    );
}

#[test]
fn eval_expr_rejects_ragged_interleaved_matrix_column() {
    let matrix = arr(
        vec![
            arr(vec![lit(1.0), lit(2.0)], false),
            arr(vec![lit(3.0), lit(4.0), lit(5.0)], false),
            lit(6.0),
        ],
        true,
    );
    let sum = rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Sum,
        args: vec![matrix],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        eval_expr::<f64>(&sum, &VarEnv::new()),
        Err(EvalError::UnsupportedExpression {
            kind: "matrix literal",
        })
    );
}

#[test]
fn eval_array_values_rejects_ragged_matrix_literal_rows() {
    let expr = arr(
        vec![
            arr(vec![lit(1.0), lit(2.0)], false),
            arr(vec![lit(3.0)], false),
        ],
        true,
    );

    assert_eq!(
        eval_array_values::<f64>(&expr, &VarEnv::new()),
        Err(EvalError::ShapeMismatch {
            context: "matrix literal",
            expected: 2,
            actual: 1,
        })
    );
}

#[test]
fn eval_matrix_values_rejects_declared_dimension_value_count_mismatch() {
    let mut env = VarEnv::<f64>::new();
    set_array_entries(&mut env, "x", &[3], &[1.0, 2.0, 3.0]);
    env.dims = Arc::new(IndexMap::from([("x".to_string(), vec![2, 2])]));

    assert_eq!(
        eval_matrix_values::<f64>(&var("x"), &env),
        Err(EvalError::ShapeMismatch {
            context: "declared array dimensions",
            expected: 4,
            actual: 3,
        })
    );
}

#[test]
fn eval_matrix_values_rejects_missing_declared_dimensions() {
    let mut env = VarEnv::<f64>::new();
    set_array_entries(&mut env, "x", &[4], &[1.0, 2.0, 3.0, 4.0]);

    assert_eq!(
        eval_matrix_values::<f64>(&var("x"), &env),
        Err(EvalError::MissingBinding {
            name: "x dimensions".to_string(),
        })
    );
}

#[test]
fn eval_expr_accepts_singleton_interleaved_matrix_column() {
    let matrix = arr(vec![arr(vec![lit(1.0), lit(2.0)], false), lit(3.0)], true);
    let sum = rumoca_core::Expression::BuiltinCall {
        function: BuiltinFunction::Sum,
        args: vec![matrix],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr::<f64>(&sum, &VarEnv::new()), Ok(9.0));
}

#[test]
fn eval_expr_rejects_missing_external_table_lookup_id_binding() {
    let lookup = fn_call(
        "getTable1DValue",
        vec![var("table_id"), int_lit(1), lit(1.0)],
    );

    assert_eq!(
        eval_expr::<f64>(&lookup, &VarEnv::new()),
        Err(EvalError::MissingBinding {
            name: "table_id".to_string(),
        })
    );
}

#[test]
fn eval_expr_rejects_unregistered_external_table_id() {
    let lookup = fn_call("getTable1DValue", vec![lit(42.0), int_lit(1), lit(1.0)]);

    assert_eq!(
        eval_expr::<f64>(&lookup, &VarEnv::new()),
        Err(EvalError::MissingBinding {
            name: "external table 42".to_string(),
        })
    );
}

#[test]
fn eval_expr_rejects_missing_external_table_lookup_input() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr_value::<f64>(&constructor, &env);
    env.set("table_id", table_id);

    let lookup = fn_call("getTable1DValue", vec![var("table_id"), int_lit(1)]);

    assert_eq!(
        eval_expr::<f64>(&lookup, &env),
        Err(EvalError::UnsupportedExpression {
            kind: "external table lookup",
        })
    );
}

#[test]
fn eval_expr_rejects_out_of_range_external_table_lookup_column() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr_value::<f64>(&constructor, &env);
    env.set("table_id", table_id);

    let lookup = fn_call(
        "getTable1DValue",
        vec![var("table_id"), int_lit(2), lit(1.0)],
    );

    assert_eq!(
        eval_expr::<f64>(&lookup, &env),
        Err(EvalError::UnsupportedExpression {
            kind: "external table lookup column",
        })
    );
}

#[test]
fn eval_expr_rejects_fractional_external_table_lookup_column() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr_value::<f64>(&constructor, &env);
    env.set("table_id", table_id);

    let lookup = fn_call("getTable1DValue", vec![var("table_id"), lit(1.5), lit(1.0)]);

    assert_eq!(
        eval_expr::<f64>(&lookup, &env),
        Err(EvalError::UnsupportedExpression {
            kind: "external table lookup column",
        })
    );
}

#[test]
fn eval_expr_accepts_valid_external_table_lookup_column() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr_value::<f64>(&constructor, &env);
    env.set("table_id", table_id);

    let lookup = fn_call(
        "getTable1DValue",
        vec![var("table_id"), int_lit(1), lit(1.0)],
    );

    assert_eq!(eval_expr::<f64>(&lookup, &env), Ok(12.0));
}

#[test]
fn fallible_external_table_helpers_reject_missing_table() {
    assert_eq!(eval_table_bound_value_opt_in(42.0, true, &[]), None);
    assert_eq!(eval_table_lookup_value_opt_in(42.0, 1.0, 1.0, &[]), None);
    assert_eq!(
        eval_table_lookup_slope_value_opt_in(42.0, 1.0, 1.0, &[]),
        None
    );
    assert_eq!(
        eval_time_table_next_event_value_opt_in(42.0, 0.0, &[]),
        None
    );
}

#[test]
fn fallible_external_table_helpers_reject_invalid_lookup_column() {
    let env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr_value::<f64>(&constructor, &env);
    let tables = external_table_data_for_parameter_values_in(&env, &[table_id]);

    assert_eq!(
        eval_table_lookup_value_opt_in(table_id, 2.0, 1.0, &tables),
        None
    );
    assert_eq!(
        eval_table_lookup_slope_value_opt_in(table_id, 1.5, 1.0, &tables),
        None
    );
}

#[test]
fn fallible_external_table_helpers_accept_valid_registered_table() {
    let env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr_value::<f64>(&constructor, &env);
    let tables = external_table_data_for_parameter_values_in(&env, &[table_id]);

    assert_eq!(
        eval_table_bound_value_opt_in(table_id, false, &tables),
        Some(0.0)
    );
    assert_eq!(
        eval_table_lookup_value_opt_in(table_id, 1.0, 1.0, &tables),
        Some(12.0)
    );
    assert_eq!(
        eval_table_lookup_slope_value_opt_in(table_id, 1.0, 1.0, &tables),
        Some(2.0)
    );
}

#[test]
fn external_table_constructor_accepts_named_table_arguments() {
    let env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            named_ctor_arg("table", simple_table_expr()),
            named_ctor_arg("columns", columns_expr()),
            named_ctor_arg("smoothness", int_lit(1)),
            named_ctor_arg("extrapolation", int_lit(1)),
        ],
    );

    let table_id = eval_expr::<f64>(&constructor, &env).expect("named table constructor lowers");
    let tables = external_table_data_for_parameter_values_in(&env, &[table_id]);

    assert_eq!(
        eval_table_lookup_value_opt_in(table_id, 1.0, 2.0, &tables),
        Some(14.0)
    );
}
